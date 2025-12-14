use egui_tiles::{Tile, Tiles, Tree};

#[test]
fn extract_and_reinsert_pane_subtree_roundtrip() {
    let mut tiles = Tiles::default();
    let a = tiles.insert_pane(1u32);
    let b = tiles.insert_pane(2u32);
    let root = tiles.insert_tab_tile(vec![a, b]);

    let mut tree = Tree::new("tree", root, tiles);
    let before_len = tree.tiles.len();

    let subtree = tree.extract_subtree(a).expect("extract subtree");
    assert!(tree.tiles.get(a).is_none());
    assert!(subtree.tiles.get(a).is_some());
    assert_eq!(tree.tiles.len() + subtree.tiles.len(), before_len);

    tree.insert_subtree_at(subtree, None);
    assert!(tree.tiles.get(a).is_some());
    assert_eq!(tree.tiles.len(), before_len);
}

#[test]
fn extract_reserves_disjoint_id_space_between_trees() {
    let mut tiles = Tiles::default();
    let a = tiles.insert_pane(());
    let b = tiles.insert_pane(());
    let root = tiles.insert_tab_tile(vec![a, b]);

    let mut root_tree = Tree::new("root", root, tiles);
    let subtree = root_tree.extract_subtree(a).expect("extract subtree");

    let mut detached_tree = Tree::new("detached", subtree.root, subtree.tiles);

    let detached_new = detached_tree.tiles.insert_new(Tile::Pane(()));
    let root_new = root_tree.tiles.insert_new(Tile::Pane(()));

    assert_ne!(detached_new, root_new, "new ids must not collide");
}

#[test]
fn multiple_extractions_get_disjoint_id_spaces() {
    let mut tiles = Tiles::default();
    let a = tiles.insert_pane(());
    let b = tiles.insert_pane(());
    let c = tiles.insert_pane(());
    let root = tiles.insert_tab_tile(vec![a, b, c]);

    let mut root_tree = Tree::new("root", root, tiles);

    let subtree_a = root_tree.extract_subtree(a).expect("extract subtree a");
    let subtree_b = root_tree.extract_subtree(b).expect("extract subtree b");

    let mut detached_a = Tree::new("a", subtree_a.root, subtree_a.tiles);
    let mut detached_b = Tree::new("b", subtree_b.root, subtree_b.tiles);

    let new_a = detached_a.tiles.insert_new(Tile::Pane(()));
    let new_b = detached_b.tiles.insert_new(Tile::Pane(()));
    let new_root = root_tree.tiles.insert_new(Tile::Pane(()));

    assert_ne!(new_a, new_b, "detached trees must not collide");
    assert_ne!(new_a, new_root, "detached vs root must not collide");
    assert_ne!(new_b, new_root, "detached vs root must not collide");
}
