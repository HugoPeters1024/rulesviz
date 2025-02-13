use std::collections::HashMap;

use eframe::egui::{self, Color32, Stroke};
use egui::RichText;
use egui_extras::{Column, TableBuilder};

use crate::toggle;

pub struct Node {
    name: String,
    inputs: Vec<InputField>,
    outputs: Vec<OutputField>,
}

#[derive(Debug, Hash, PartialEq, Eq, Clone)]
pub struct InputRef {
    window_id: egui::Id,
    idx: usize,
}

#[derive(Debug, Hash, PartialEq, Eq, Clone)]
pub struct OutputRef {
    window_id: egui::Id,
    idx: usize,
}

#[derive(PartialEq, Eq, Hash, Clone)]
enum NodeRef {
    Input(InputRef),
    Output(OutputRef),
}

impl Node {
    fn show(&mut self, ctx: &egui::Context, id: &egui::Id) -> Option<NodeRef> {
        let mut interaction = None;
        egui::Window::new(&self.name)
            .id(*id)
            .resizable(false)
            .collapsible(false)
            .show(ctx, |ui| {
                let row_count = self.inputs.len().max(self.outputs.len());
                TableBuilder::new(ui)
                    .column(Column::exact(256.0).resizable(false))
                    .column(Column::remainder().resizable(false))
                    .body(|mut body| {
                        for i in 0..row_count {
                            body.row(20.0, |mut row| {
                                row.col(|ui| {
                                    if i < self.inputs.len() && self.inputs[i].show(ui) {
                                        interaction = Some(NodeRef::Input(InputRef {
                                            window_id: *id,
                                            idx: i,
                                        }))
                                    }
                                });
                                row.col(|ui| {
                                    if i < self.outputs.len() && self.outputs[i].show(ui) {
                                        interaction = Some(NodeRef::Output(OutputRef {
                                            window_id: *id,
                                            idx: i,
                                        }))
                                    }
                                });
                            });
                        }
                    });
            });

        interaction
    }

    fn project_fields(fields: &[&str]) -> Self {
        Node {
            name: "project fields".to_string(),
            inputs: vec![],
            outputs: fields.iter().map(|x| OutputField::new(x)).collect(),
        }
    }

    fn output_fields(fields: &[&str]) -> Self {
        Node {
            name: "output fields".to_string(),
            inputs: fields.iter().map(|x| InputField::field_only(x)).collect(),
            outputs: vec![],
        }
    }

    fn strip_html() -> Self {
        Node {
            name: "strip html".to_string(),
            inputs: vec![InputField::field_only("input")],
            outputs: vec![OutputField::new("output")],
        }
    }

    fn concatenate() -> Self {
        Node {
            name: "concatenate".to_string(),
            inputs: vec![
                InputField::field_only("left"),
                InputField::field_or_value("right"),
            ],
            outputs: vec![OutputField::new("output")],
        }
    }
}

#[derive(PartialEq, Eq, Default)]
pub struct FieldOrValue {
    value: String,
    toggle: bool,
}

#[derive(PartialEq, Eq)]
pub enum InputKind {
    FieldOnly,
    ValueOnly { value: String },
    FieldOrValue(FieldOrValue),
}

pub struct InputField {
    kind: InputKind,
    name: String,
    position: egui::Pos2,
}

impl InputField {
    fn field_only(name: &str) -> Self {
        InputField {
            kind: InputKind::FieldOnly,
            name: name.to_string(),
            position: egui::Pos2::default(),
        }
    }

    fn field_or_value(name: &str) -> Self {
        InputField {
            kind: InputKind::FieldOrValue(FieldOrValue::default()),
            name: name.to_string(),
            position: egui::Pos2::default(),
        }
    }

    fn is_currently_for_field(&self) -> bool {
        match &self.kind {
            InputKind::FieldOnly => true,
            InputKind::ValueOnly { .. } => false,
            InputKind::FieldOrValue(field_or_value) => !field_or_value.toggle,
        }
    }

    fn show(&mut self, ui: &mut egui::Ui) -> bool {
        let mut clicked = false;
        ui.with_layout(
            egui::Layout::left_to_right(egui::Align::LEFT),
            |ui| match &mut self.kind {
                InputKind::FieldOnly
                | InputKind::FieldOrValue(FieldOrValue { toggle: false, .. }) => {
                    let response = ui.add(egui::RadioButton::new(false, ""));
                    clicked = response.clicked();
                    self.position = response.rect.center();
                    ui.label(&self.name);

                    match &mut self.kind {
                        InputKind::FieldOrValue(field_or_value) => {
                            ui.add(toggle::toggle(&mut field_or_value.toggle));
                            ui.label(RichText::new("static value").italics());
                        }
                        _ => {}
                    }
                }
                InputKind::ValueOnly { value }
                | InputKind::FieldOrValue(FieldOrValue {
                    toggle: true,
                    value,
                }) => {
                    ui.add(
                        egui::TextEdit::singleline(value)
                            .clip_text(true)
                            .desired_width(128.0),
                    );

                    match &mut self.kind {
                        InputKind::FieldOrValue(field_or_value) => {
                            ui.add(toggle::toggle(&mut field_or_value.toggle));
                            ui.label(RichText::new("static value").italics());
                        }
                        _ => {}
                    }
                }
            },
        );
        clicked
    }
}

pub struct OutputField {
    name: String,
    position: egui::Pos2,
}

impl OutputField {
    fn new(name: &str) -> Self {
        OutputField {
            name: name.to_string(),
            position: egui::Pos2::default(),
        }
    }

    fn show(&mut self, ui: &mut egui::Ui) -> bool {
        let mut clicked = false;
        ui.with_layout(egui::Layout::right_to_left(egui::Align::LEFT), |ui| {
            let response = ui.add(egui::RadioButton::new(false, ""));
            clicked = response.clicked();
            self.position = response.rect.center();
            ui.label(&self.name);
        });

        clicked
    }
}

pub struct MyApp {
    nodes: HashMap<egui::Id, Node>,
    input_slot: Option<InputRef>,
    output_slot: Option<OutputRef>,
    edges: HashMap<InputRef, OutputRef>,
    show_dropdown: bool,
    next_id_val: u64,
}

impl MyApp {
    pub fn new() -> Self {
        let mut ret = Self::default();
        ret.add_node(Node::project_fields(&[
            "title",
            "description",
            "price",
            "stock",
        ]));
        ret.add_node(Node::output_fields(&[
            "amazon_title",
            "description",
            "price",
            "stock",
        ]));
        ret
    }

    pub fn next_id(&mut self) -> egui::Id {
        self.next_id_val += 1;
        egui::Id::new(self.next_id_val)
    }

    pub fn add_node(&mut self, node: Node) {
        let next_id = self.next_id();
        self.nodes.insert(next_id, node);
    }

    pub fn remove_impossible_edges(&mut self) {
        let retain_edges = self
            .edges
            .iter()
            .map(|(lhs, _)| {
                (lhs.clone(), {
                    if let Some(input) = self.resolve_input(lhs) {
                        input.is_currently_for_field()
                    } else {
                        false
                    }
                })
            })
            .collect::<HashMap<_, _>>();
        self.edges.retain(|lhs, _| retain_edges[lhs]);
    }

    pub fn resolve_input(&self, node_ref: &InputRef) -> Option<&InputField> {
        self.nodes
            .get(&node_ref.window_id)
            .map(|node| &node.inputs[node_ref.idx])
    }

    pub fn resolve_output(&self, node_ref: &OutputRef) -> Option<&OutputField> {
        self.nodes
            .get(&node_ref.window_id)
            .map(|node| &node.outputs[node_ref.idx])
    }
}

impl Default for MyApp {
    fn default() -> Self {
        Self {
            nodes: HashMap::new(),
            input_slot: None,
            output_slot: None,
            edges: HashMap::new(),
            show_dropdown: false,
            next_id_val: 0,
        }
    }
}

impl eframe::App for MyApp {
    fn update(&mut self, ctx: &egui::Context, _frame: &mut eframe::Frame) {
        for (id, action_node) in self.nodes.iter_mut() {
            if let Some(interaction) = action_node.show(ctx, id) {
                match interaction {
                    NodeRef::Input(input_ref) => self.input_slot = Some(input_ref),
                    NodeRef::Output(output_ref) => self.output_slot = Some(output_ref),
                }
            }
        }

        if self.input_slot.is_some() && self.output_slot.is_some() {
            let lhs = self.input_slot.take().unwrap();
            let rhs = self.output_slot.take().unwrap();
            if lhs.window_id != rhs.window_id {
                self.edges.insert(lhs, rhs);
            }
        }

        if self.input_slot.is_some() ^ self.output_slot.is_some() {
            // Draw the line in an overlay
            egui::Area::new(egui::Id::new("overlay"))
                .order(egui::Order::Foreground)
                .show(ctx, |ui| {
                    let window_pos = match (&self.input_slot, &self.output_slot) {
                        (None, Some(output)) => self.resolve_output(output).unwrap().position,
                        (Some(input), None) => self.resolve_input(input).unwrap().position,
                        _ => panic!("impossible"),
                    };

                    if let Some(mouse_pos) = ctx.pointer_latest_pos() {
                        let painter = ui.painter();
                        painter.line_segment(
                            [window_pos, mouse_pos],
                            Stroke::new(2.0, Color32::WHITE),
                        );
                    }
                });
        }

        // Draw the lines in an overlay
        egui::Area::new(egui::Id::new("overlay"))
            .order(egui::Order::Foreground)
            .show(ctx, |ui| {
                for (lhs, rhs) in &self.edges {
                    let lhs = self.resolve_input(lhs).unwrap();
                    let rhs = self.resolve_output(rhs).unwrap();

                    let painter = ui.painter();
                    painter.line_segment(
                        [lhs.position, rhs.position],
                        Stroke::new(2.0, Color32::WHITE),
                    );
                }
            });

        egui::CentralPanel::default().show(ctx, |ui| {
            let mut set_dropdown_location = false;

            // Check for right-click
            if ui.input(|i| i.pointer.button_down(egui::PointerButton::Secondary))
                && !self.show_dropdown
            {
                self.show_dropdown = true;
                set_dropdown_location = true;
            }

            // Show dropdown if right-clicked
            if self.show_dropdown {
                let mut window = egui::Window::new("Add Node")
                    .order(egui::Order::Foreground)
                    .resizable(false)
                    .collapsible(false);

                if set_dropdown_location {
                    window = window
                        .fixed_pos(ui.input(|i| i.pointer.interact_pos().unwrap_or_default()));
                }
                let response = window.show(ctx, |ui| {
                    for node in [Node::strip_html(), Node::concatenate()] {
                        if ui.button(&node.name).clicked() {
                            let id = self.next_id();
                            self.nodes.insert(id, node);
                            self.show_dropdown = false;
                        }
                    }
                });

                // Close the dropdown if the user clicks outside of it
                if response.unwrap().response.clicked_elsewhere() {
                    self.show_dropdown = false;
                }
            }
        });

        self.remove_impossible_edges();
    }
}
