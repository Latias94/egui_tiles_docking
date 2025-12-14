# `egui_tiles_docking`

[<img alt="github" src="https://img.shields.io/badge/github-Latias94/egui__tiles__docking-8da0cb?logo=github" height="20">](https://github.com/Latias94/egui_tiles_docking)
[![Latest version](https://img.shields.io/crates/v/egui_tiles_docking.svg)](https://crates.io/crates/egui_tiles_docking)
[![Documentation](https://docs.rs/egui_tiles_docking/badge.svg)](https://docs.rs/egui_tiles_docking)
[![unsafe forbidden](https://img.shields.io/badge/unsafe-forbidden-success.svg)](https://github.com/rust-secure-code/safety-dance/)
[![Build Status](https://github.com/Latias94/egui_tiles_docking/actions/workflows/rust.yml/badge.svg)](https://github.com/Latias94/egui_tiles_docking/actions/workflows/rust.yml)
[![MIT](https://img.shields.io/badge/license-MIT-blue.svg)](https://github.com/Latias94/egui_tiles_docking/blob/main/LICENSE-MIT)
[![Apache](https://img.shields.io/badge/license-Apache-blue.svg)](https://github.com/Latias94/egui_tiles_docking/blob/main/LICENSE-APACHE)

This is a fork of [`rerun-io/egui_tiles`](https://github.com/rerun-io/egui_tiles) with a few additional public APIs,
primarily to support bridging docking models to native multi-window ("multi-viewport") workflows.

Layouting and docking for [egui](https://github.com/emilk/egui).

## Usage
To use this fork as a drop-in replacement for `egui_tiles`:
```toml
egui_tiles = { package = "egui_tiles_docking", version = "0.14" }
```

Supports:
* Horizontal and vertical layouts
* Grid layouts
* Tabs
* Drag-and-drop docking

![egui_tiles](https://github.com/rerun-io/egui_tiles/assets/1148717/f86bee40-2506-4484-8a82-37ffdc805b81)

### Trying it
`cargo r --example simple`

### Comparison with [egui_dock](https://github.com/Adanos020/egui_dock)
[egui_dock](https://github.com/Adanos020/egui_dock) is an excellent crate serving similar needs. `egui_tiles` aims to become a more flexible and feature-rich alternative to `egui_dock`.

`egui_dock` only supports binary splits (left/right or top/bottom), while `egui_tiles` support full horizontal and vertical layouts, as well as grid layouts. `egui_tiles` also strives to be more customizable, enabling users to override the default style and behavior by implementing methods on a `Behavior` `trait`.

`egui_dock` supports some features that `egui_tiles` does not yet support, such as close-buttons on each tab, and built-in scroll areas.

---

<div align="center">
<img src="https://user-images.githubusercontent.com/1148717/236840584-f4795fb3-89e3-40ac-b570-ac2869e6e8fa.png" width="50%">

Upstream `egui_tiles` development is sponsored by [Rerun](https://www.rerun.io/), a startup doing<br>
visualizations for computer vision and robotics.
</div>
