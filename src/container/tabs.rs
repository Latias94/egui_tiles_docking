use egui::{NumExt as _, Rect, Vec2, scroll_area::ScrollBarVisibility, vec2};

use crate::behavior::{EditAction, TabState, tab_close_requested_id};
use crate::{
    Behavior, ContainerInsertion, DropContext, InsertionPoint, SimplifyAction, Tile, TileId, Tiles,
    Tree, is_being_dragged,
};

/// Fixed size icons for `⏴` and `⏵`
const SCROLL_ARROW_SIZE: Vec2 = Vec2::splat(20.0);

/// A container with tabs. Only one tab is open (active) at a time.
#[derive(Clone, Debug, Default, PartialEq, Eq)]
#[cfg_attr(feature = "serde", derive(serde::Deserialize, serde::Serialize))]
pub struct Tabs {
    /// The tabs, in order.
    pub children: Vec<TileId>,

    /// The currently open tab.
    pub active: Option<TileId>,
}

/// The current tab scrolling state
#[derive(Clone, Copy, Debug, Default)]
struct ScrollState {
    /// The current horizontal scroll offset.
    ///
    /// Positive: scroll right.
    /// Negatie: scroll left.
    pub offset: f32,

    /// Outstanding offset to apply smoothly over the next few frames.
    /// This is what the buttons update.
    pub offset_debt: f32,

    /// The size of all the tabs last frame.
    pub content_size: Vec2,

    /// The available size for the tabs.
    pub available: Vec2,

    /// Show the left scroll-arrow this frame?
    pub show_left_arrow: bool,

    /// Show the right scroll-arrow this frame?
    pub show_right_arrow: bool,

    /// Did we show the left scroll-arrow last frame?
    pub showed_left_arrow_prev: bool,

    /// Did we show the right scroll-arrow last frame?
    pub showed_right_arrow_prev: bool,
}

/// Tracks hover time on a tab during an active drag to allow delayed auto-selection (ImGui-like).
#[derive(Clone, Copy, Debug, Default)]
struct DragHoverSwitch {
    hovered: Option<TileId>,
    seconds: f32,
}

impl DragHoverSwitch {
    fn reset(&mut self) {
        self.hovered = None;
        self.seconds = 0.0;
    }

    fn update(&mut self, hovered: Option<TileId>, dt: f32) {
        if hovered.is_none() {
            self.reset();
            return;
        }

        if self.hovered != hovered {
            self.hovered = hovered;
            self.seconds = dt.min(10.0);
        } else {
            self.seconds = (self.seconds + dt).min(10.0);
        }
    }

    fn ready_target(self, active: Option<TileId>, delay_seconds: f32) -> Option<TileId> {
        if !(delay_seconds.is_finite() && delay_seconds > 0.0) {
            return None;
        }
        let hovered = self.hovered?;
        (Some(hovered) != active && self.seconds >= delay_seconds).then_some(hovered)
    }
}

#[cfg(test)]
mod drag_hover_switch_tests {
    use super::*;

    #[test]
    fn switches_after_delay_and_resets_on_change() {
        let a = TileId::from_u64(1);
        let b = TileId::from_u64(2);

        let mut s = DragHoverSwitch::default();
        let delay = 0.15;

        // Hover A for less than delay: no switch.
        s.update(Some(a), 0.10);
        assert_eq!(s.ready_target(Some(b), delay), None);

        // Cross the threshold: switch.
        s.update(Some(a), 0.06);
        assert_eq!(s.ready_target(Some(b), delay), Some(a));

        // Change hovered tab: timer resets.
        s.update(Some(b), 0.14);
        assert_eq!(s.ready_target(Some(a), delay), None);
        s.update(Some(b), 0.02);
        assert_eq!(s.ready_target(Some(a), delay), Some(b));

        // No hovered tab: reset.
        s.update(None, 0.10);
        assert_eq!(s.ready_target(Some(a), delay), None);
    }
}

impl ScrollState {
    /// Returns the space left for the tabs after the scroll arrows.
    pub fn update(&mut self, ui: &egui::Ui) -> f32 {
        let mut scroll_area_width = ui.available_width();

        let button_and_spacing_width = SCROLL_ARROW_SIZE.x + ui.spacing().item_spacing.x;

        let margin = 0.1;

        self.show_left_arrow = SCROLL_ARROW_SIZE.x < self.offset;

        if self.show_left_arrow {
            scroll_area_width -= button_and_spacing_width;
        }

        self.show_right_arrow = self.offset + scroll_area_width + margin < self.content_size.x;

        // Compensate for showing/hiding of arrow:
        self.offset += button_and_spacing_width
            * ((self.show_left_arrow as i32 as f32) - (self.showed_left_arrow_prev as i32 as f32));

        if self.show_right_arrow {
            scroll_area_width -= button_and_spacing_width;
        }

        self.showed_left_arrow_prev = self.show_left_arrow;
        self.showed_right_arrow_prev = self.show_right_arrow;

        if self.offset_debt != 0.0 {
            const SPEED: f32 = 500.0;

            let dt = ui.input(|i| i.stable_dt).min(0.1);
            let max_movement = dt * SPEED;
            if self.offset_debt.abs() <= max_movement {
                self.offset += self.offset_debt;
                self.offset_debt = 0.0;
            } else {
                let movement = self.offset_debt.signum() * max_movement;
                self.offset += movement;
                self.offset_debt -= movement;
                ui.ctx().request_repaint();
            }
        }

        scroll_area_width
    }

    fn scroll_increment(&self) -> f32 {
        (self.available.x / 3.0).at_least(20.0)
    }

    pub fn left_arrow(&mut self, ui: &mut egui::Ui) {
        if !self.show_left_arrow {
            return;
        }

        if ui
            .add_sized(SCROLL_ARROW_SIZE, egui::Button::new("⏴"))
            .clicked()
        {
            self.offset_debt -= self.scroll_increment();
        }
    }

    pub fn right_arrow(&mut self, ui: &mut egui::Ui) {
        if !self.show_right_arrow {
            return;
        }

        if ui
            .add_sized(SCROLL_ARROW_SIZE, egui::Button::new("⏵"))
            .clicked()
        {
            self.offset_debt += self.scroll_increment();
        }
    }
}

impl Tabs {
    pub fn new(children: Vec<TileId>) -> Self {
        let active = children.first().copied();
        Self { children, active }
    }

    pub fn add_child(&mut self, child: TileId) {
        self.children.push(child);
    }

    pub fn set_active(&mut self, child: TileId) {
        self.active = Some(child);
    }

    pub fn is_active(&self, child: TileId) -> bool {
        Some(child) == self.active
    }

    pub(super) fn layout<Pane>(
        &mut self,
        tiles: &mut Tiles<Pane>,
        style: &egui::Style,
        behavior: &mut dyn Behavior<Pane>,
        rect: Rect,
    ) {
        let prev_active = self.active;
        self.ensure_active(tiles);
        if prev_active != self.active {
            behavior.on_edit(EditAction::TabSelected);
        }

        let tab_bar_height = if behavior.auto_hide_tab_bar_when_single_tab()
            && self.visible_children_count(tiles) <= 1
        {
            0.0
        } else {
            behavior.tab_bar_height(style)
        };

        let mut active_rect = rect;
        active_rect.min.y += tab_bar_height;

        if let Some(active) = self.active {
            // Only lay out the active tab (saves CPU):
            tiles.layout_tile(style, behavior, active_rect, active);
        }
    }

    pub fn next_active<Pane>(&self, tiles: &Tiles<Pane>) -> Option<TileId> {
        self.active
            .filter(|active| {
                self.children.contains(active)
                    && tiles.get(*active).is_some()
                    && tiles.is_visible(*active)
            })
            .or_else(|| {
                self.children
                    .iter()
                    .copied()
                    .find(|&child_id| tiles.get(child_id).is_some() && tiles.is_visible(child_id))
            })
    }

    /// Make sure we have an active tab (or no visible tabs).
    pub fn ensure_active<Pane>(&mut self, tiles: &Tiles<Pane>) {
        self.active = self.next_active(tiles);
    }

    pub(super) fn ui<Pane>(
        &mut self,
        tree: &mut Tree<Pane>,
        behavior: &mut dyn Behavior<Pane>,
        drop_context: &mut DropContext,
        ui: &mut egui::Ui,
        rect: Rect,
        tile_id: TileId,
    ) {
        let show_tab_bar = !(behavior.auto_hide_tab_bar_when_single_tab()
            && self.visible_children_count(&tree.tiles) <= 1);
        let next_active = if show_tab_bar {
            self.tab_bar_ui(tree, behavior, ui, rect, drop_context, tile_id)
        } else {
            self.next_active(&tree.tiles)
        };

        if let Some(active) = self.active {
            tree.tile_ui(behavior, drop_context, ui, active);
            crate::cover_tile_if_dragged(tree, behavior, ui, active);
        }

        // We have only laid out the active tab, so we need to switch active tab _after_ the ui pass above:
        self.active = next_active;
    }

    fn visible_children_count<Pane>(&self, tiles: &Tiles<Pane>) -> usize {
        self.children
            .iter()
            .copied()
            .filter(|&child_id| tiles.get(child_id).is_some() && tiles.is_visible(child_id))
            .count()
    }

    /// Returns the next active tab (e.g. the one clicked, or the current).
    #[allow(clippy::too_many_lines)]
    fn tab_bar_ui<Pane>(
        &mut self,
        tree: &mut Tree<Pane>,
        behavior: &mut dyn Behavior<Pane>,
        ui: &mut egui::Ui,
        rect: Rect,
        drop_context: &mut DropContext,
        tile_id: TileId,
    ) -> Option<TileId> {
        let mut next_active = self.active;
        let mut close_requested: Option<TileId> = None;

        let tab_bar_height = behavior.tab_bar_height(ui.style());
        let tab_bar_rect = rect.split_top_bottom_at_y(rect.top() + tab_bar_height).0;
        let mut ui = ui.new_child(egui::UiBuilder::new().max_rect(tab_bar_rect));

        let mut button_rects = ahash::HashMap::default();
        let mut dragged_index = None;

        ui.painter()
            .rect_filled(ui.max_rect(), 0.0, behavior.tab_bar_color(ui.visuals()));

        let scroll_state_id = ui.make_persistent_id(tile_id);
        let drag_hover_switch_id = scroll_state_id.with("drag_hover_switch");
        let mut scroll_state = ui.ctx().memory_mut(|m| {
            m.data
                .get_temp::<ScrollState>(scroll_state_id)
                .unwrap_or_default()
        });

        // Left-side top-bar UI (left-to-right).
        let left_used = {
            let mut left_ui = ui.new_child(
                egui::UiBuilder::new()
                    .max_rect(tab_bar_rect)
                    .layout(egui::Layout::left_to_right(egui::Align::Center)),
            );
            left_ui.push_id("tab_bar_left", |ui| {
                behavior.top_bar_left_ui(&tree.tiles, ui, tile_id, self, &mut scroll_state.offset);
            });
            (left_ui.cursor().min.x - tab_bar_rect.min.x).at_least(0.0)
        };

        // Right-side top-bar UI (right-to-left).
        let right_used = {
            let mut right_ui = ui.new_child(
                egui::UiBuilder::new()
                    .max_rect(tab_bar_rect)
                    .layout(egui::Layout::right_to_left(egui::Align::Center)),
            );
            right_ui.push_id("tab_bar_right", |ui| {
                behavior.top_bar_right_ui(&tree.tiles, ui, tile_id, self, &mut scroll_state.offset);
            });
            (tab_bar_rect.max.x - right_ui.cursor().max.x).at_least(0.0)
        };

        // Center area: tabs + scroll arrows.
        let mut center_rect = tab_bar_rect;
        center_rect.min.x = (center_rect.min.x + left_used).min(center_rect.max.x);
        center_rect.max.x = (center_rect.max.x - right_used).max(center_rect.min.x);

        let mut center_ui = ui.new_child(egui::UiBuilder::new().max_rect(center_rect));
        center_ui.with_layout(egui::Layout::right_to_left(egui::Align::Center), |ui| {
            let scroll_area_width = scroll_state.update(ui);

            // We're in a right-to-left layout, so start with the right scroll-arrow:
            scroll_state.right_arrow(ui);

            ui.allocate_ui_with_layout(
                ui.available_size(),
                egui::Layout::left_to_right(egui::Align::Center),
                |ui| {
                    scroll_state.left_arrow(ui);

                    // Prepare to show the scroll area with the tabs:

                    scroll_state.offset = scroll_state
                        .offset
                        .at_most(scroll_state.content_size.x - ui.available_width());
                    scroll_state.offset = scroll_state.offset.at_least(0.0);

                    let scroll_area = egui::ScrollArea::horizontal()
                        .scroll_bar_visibility(ScrollBarVisibility::AlwaysHidden)
                        .max_width(scroll_area_width)
                        .auto_shrink([false; 2])
                        .horizontal_scroll_offset(scroll_state.offset);

                    let mut hovered_tab_during_drag: Option<TileId> = None;
                    let output = scroll_area.show(ui, |ui| {
                        ui.spacing_mut().item_spacing.x = 0.0; // Tabs have spacing built-in

                        for (i, &child_id) in self.children.iter().enumerate() {
                            if !tree.is_visible(child_id) {
                                continue;
                            }

                            let is_being_dragged = is_being_dragged(ui.ctx(), tree.id, child_id);

                            let selected = self.is_active(child_id);
                            let id = child_id.egui_id(tree.id);
                            let tab_state = TabState {
                                active: selected,
                                is_being_dragged,
                                closable: behavior.is_tab_closable(&tree.tiles, child_id),
                            };

                            let response =
                                behavior.tab_ui(&mut tree.tiles, ui, id, child_id, &tab_state);

                            let requested = ui.ctx().data(|d| {
                                d.get_temp::<bool>(tab_close_requested_id(id))
                                    .unwrap_or(false)
                            });
                            if requested {
                                close_requested = Some(child_id);
                                ui.ctx()
                                    .data_mut(|d| d.insert_temp(tab_close_requested_id(id), false));
                            }

                            if response.clicked() {
                                behavior.on_edit(EditAction::TabSelected);
                                next_active = Some(child_id);
                            }

                            if let Some(mouse_pos) = drop_context.mouse_pos {
                                if drop_context.dragged_tile_id.is_some()
                                    && response.rect.contains(mouse_pos)
                                {
                                    hovered_tab_during_drag = Some(child_id);
                                }
                            }

                            button_rects.insert(child_id, response.rect);
                            tree.tiles.tab_rects.insert(child_id, response.rect);
                            if is_being_dragged {
                                dragged_index = Some(i);
                            }
                        }
                    });

                    scroll_state.offset = output.state.offset.x;
                    scroll_state.content_size = output.content_size;
                    scroll_state.available = output.inner_rect.size();

                    // ImGui-like: dragging empty background space in the tab bar drags the whole dock node.
                    //
                    // IMPORTANT: do not overlap this draggable region with the actual tab buttons, otherwise
                    // it will steal drags and prevent per-tab dragging (which is required to re-dock a tab
                    // from a detached viewport back into another dock tree).
                    let visible_children_are_all_panes = self
                        .children
                        .iter()
                        .copied()
                        .filter(|&child| {
                            tree.tiles.get(child).is_some() && tree.tiles.is_visible(child)
                        })
                        .all(|child| matches!(tree.tiles.get(child), Some(Tile::Pane(_))));

                    if visible_children_are_all_panes {
                        let inner_rect = output.inner_rect;
                        let (min_x, max_x) = button_rects.values().fold(
                            (inner_rect.max.x, inner_rect.min.x),
                            |(min_x, max_x), rect| (min_x.min(rect.min.x), max_x.max(rect.max.x)),
                        );

                        let mut bg_rects = Vec::new();
                        if button_rects.is_empty() {
                            bg_rects.push(inner_rect);
                        } else {
                            let left_max_x = min_x.clamp(inner_rect.min.x, inner_rect.max.x);
                            let right_min_x = max_x.clamp(inner_rect.min.x, inner_rect.max.x);
                            if left_max_x > inner_rect.min.x {
                                bg_rects.push(Rect::from_min_max(
                                    inner_rect.min,
                                    egui::pos2(left_max_x, inner_rect.max.y),
                                ));
                            }
                            if right_min_x < inner_rect.max.x {
                                bg_rects.push(Rect::from_min_max(
                                    egui::pos2(right_min_x, inner_rect.min.y),
                                    inner_rect.max,
                                ));
                            }
                        }

                        for (i, bg_rect) in bg_rects.into_iter().enumerate() {
                            let bg_id = ui.id().with(("tab_bar_bg_drag", i));
                            let response = ui
                                .interact(bg_rect, bg_id, egui::Sense::click_and_drag())
                                .on_hover_cursor(egui::CursorIcon::Grab);
                            if response.drag_started() {
                                behavior.on_edit(EditAction::TileDragged);
                                ui.ctx().set_dragged_id(tile_id.egui_id(tree.id));
                            }
                            if response.double_clicked() {
                                let id =
                                    crate::tab_bar_background_double_clicked_id(tree.id, tile_id);
                                ui.ctx().data_mut(|d| d.insert_temp(id, true));
                            }
                        }
                    }

                    // ImGui-like: during an active drag, only auto-select a tab after a short hover delay.
                    let dt = ui.input(|i| i.stable_dt).min(0.1);
                    let delay = behavior.tab_switch_on_drag_hover_delay();
                    let mut hover_switch = ui
                        .ctx()
                        .data(|d| d.get_temp::<DragHoverSwitch>(drag_hover_switch_id))
                        .unwrap_or_default();

                    if drop_context.dragged_tile_id.is_some() {
                        hover_switch.update(hovered_tab_during_drag, dt);
                        if let Some(target) = hover_switch.ready_target(self.active, delay) {
                            behavior.on_edit(EditAction::TabSelected);
                            next_active = Some(target);
                            hover_switch.seconds = 0.0;
                        }
                    } else {
                        hover_switch.reset();
                    }

                    ui.ctx()
                        .data_mut(|d| d.insert_temp(drag_hover_switch_id, hover_switch));
                },
            );
        });

        ui.ctx()
            .data_mut(|data| data.insert_temp(scroll_state_id, scroll_state));

        if let Some(tile_id) = close_requested {
            if behavior.on_tab_close(&mut tree.tiles, tile_id) {
                // NOTE: during `Tree::tile_ui`, container tiles are typically moved out of `tree.tiles`
                // temporarily, so calling `tree.remove_recursively` here may fail to update the parent
                // container. We therefore remove the subtree directly from `tree.tiles` and update
                // `self.children` explicitly.
                fn remove_subtree_from_tiles<Pane>(tiles: &mut Tiles<Pane>, root: TileId) {
                    let mut stack = vec![root];
                    while let Some(tile_id) = stack.pop() {
                        // Clean up visibility bookkeeping even if the tile is already gone:
                        tiles.set_visible(tile_id, true);

                        let Some(tile) = tiles.remove(tile_id) else {
                            continue;
                        };
                        if let Tile::Container(container) = tile {
                            stack.extend(container.children().copied());
                        }
                    }
                }

                remove_subtree_from_tiles(&mut tree.tiles, tile_id);
                self.children.retain(|&id| id != tile_id);
                if self.active == Some(tile_id) {
                    self.active = None;
                }
                self.ensure_active(&tree.tiles);
                ui.ctx().request_repaint();
            }
            return self.active;
        }

        // -----------
        // Drop zones:

        let preview_thickness = 6.0;
        let after_rect = |rect: Rect| {
            let dragged_size = if let Some(dragged_index) = dragged_index {
                // We actually know the size of this thing
                button_rects[&self.children[dragged_index]].size()
            } else {
                rect.size() // guess that the size is the same as the last button
            };
            Rect::from_min_size(
                rect.right_top() + vec2(ui.spacing().item_spacing.x, 0.0),
                dragged_size,
            )
        };
        super::linear::drop_zones(
            preview_thickness,
            &self.children,
            dragged_index,
            super::LinearDir::Horizontal,
            |tile_id| button_rects.get(&tile_id).copied(),
            |rect, i| {
                drop_context.suggest_rect(
                    InsertionPoint::new(tile_id, ContainerInsertion::Tabs(i)),
                    rect,
                );
            },
            after_rect,
        );

        next_active
    }

    pub(super) fn simplify_children(&mut self, mut simplify: impl FnMut(TileId) -> SimplifyAction) {
        self.children.retain_mut(|child| match simplify(*child) {
            SimplifyAction::Remove => false,
            SimplifyAction::Keep => true,
            SimplifyAction::Replace(new) => {
                if self.active == Some(*child) {
                    self.active = Some(new);
                }
                *child = new;
                true
            }
        });
    }

    /// Returns child index, if found.
    pub(crate) fn remove_child(&mut self, needle: TileId) -> Option<usize> {
        let index = self.children.iter().position(|&child| child == needle)?;
        self.children.remove(index);
        Some(index)
    }
}
