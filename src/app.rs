use std::collections::{HashMap, VecDeque};
use voca_rs::*;

use eframe::egui::{self, Color32};
use egui::{ahash::HashSet, Pos2, RichText};
use egui_extras::{Column, TableBuilder};
use strum::IntoEnumIterator;
use strum_macros::EnumIter;

use crate::toggle;

pub struct Item(pub HashMap<String, String>);

pub enum FieldEvent {
    LeftClicked,
    RightClicked,
}

#[derive(PartialEq, Eq, EnumIter)]
pub enum NodeKind {
    ProjectFields,
    OutputFields,
    StripHtml,
    Concatenate,
    SetToValue,
    CalculateSum,
    SearchAndReplace,
    IfEqual,
    IfContains,
    Filter,
    Supervisor,
}

impl NodeKind {
    fn name(&self) -> &str {
        match self {
            NodeKind::ProjectFields => "project fields",
            NodeKind::OutputFields => "output fields",
            NodeKind::StripHtml => "strip html",
            NodeKind::Concatenate => "concatenate",
            NodeKind::SetToValue => "set to value",
            NodeKind::CalculateSum => "calculate sum",
            NodeKind::SearchAndReplace => "search and replace",
            NodeKind::IfEqual => "if equal",
            NodeKind::IfContains => "if contains",
            NodeKind::Supervisor => "supervisor",
            NodeKind::Filter => "filter",
        }
    }

    fn is_action(&self) -> bool {
        match self {
            NodeKind::ProjectFields => false,
            NodeKind::OutputFields => false,
            _ => true,
        }
    }
}

pub struct Node {
    kind: NodeKind,
    inputs: Vec<InputField>,
    outputs: Vec<OutputField>,
    is_closed: bool,
}

#[derive(Debug, Hash, PartialEq, Eq, Clone)]
pub struct InputRef {
    window_id: egui::Id,
    field_idx: usize,
}

#[derive(Debug, Hash, PartialEq, Eq, Clone)]
pub struct OutputRef {
    window_id: egui::Id,
    field_idx: usize,
}

#[derive(PartialEq, Eq, Hash, Clone)]
pub enum NodeRef {
    Input(InputRef),
    Output(OutputRef),
}

pub enum NodeEvent {
    FieldEvent {
        inner: FieldEvent,
        node_ref: NodeRef,
    },
}

impl Node {
    fn name(&self) -> &str {
        self.kind.name()
    }

    fn clear_output_previews(&mut self) {
        for output in self.outputs.iter_mut() {
            output.output_preview.clear();
        }
    }

    fn process_preview(&mut self, inputs: &Vec<String>) {
        assert!(self.inputs.len() == inputs.len());
        match self.kind {
            NodeKind::ProjectFields => {
                for (lhs, rhs) in inputs.iter().zip(self.outputs.iter_mut()) {
                    rhs.output_preview.push(lhs.clone());
                }
            }
            NodeKind::OutputFields => {
                for (lhs, rhs) in inputs.iter().zip(self.outputs.iter_mut()) {
                    rhs.output_preview.push(lhs.clone());
                }
            }
            NodeKind::StripHtml => {
                assert!(self.outputs.len() == 1);
                self.outputs[0]
                    .output_preview
                    .push(strip::strip_tags(&inputs[0]));
            }
            NodeKind::Concatenate => {
                let lhs = &inputs[0];
                let rhs = &inputs[1];
                self.outputs[0]
                    .output_preview
                    .push(format!("{}{}", lhs, rhs));
            }
            NodeKind::SetToValue => {
                self.outputs[0].output_preview.push(inputs[0].clone());
            }
            NodeKind::CalculateSum => {
                let lhs = inputs[0].parse::<f64>().unwrap_or(f64::NAN);
                let rhs = inputs[1].parse::<f64>().unwrap_or(f64::NAN);
                self.outputs[0]
                    .output_preview
                    .push(format!("{}", lhs + rhs));
            }
            NodeKind::IfEqual => {
                let discr = &inputs[0];
                let test = &inputs[1];
                self.outputs[0].output_preview.push(if discr == test {
                    inputs[2].clone()
                } else {
                    inputs[3].clone()
                });
            }
            NodeKind::IfContains => {
                let discr = &inputs[0];
                let test = &inputs[1];
                self.outputs[0]
                    .output_preview
                    .push(if discr.contains(test) {
                        inputs[2].clone()
                    } else {
                        inputs[3].clone()
                    });
            }
            NodeKind::SearchAndReplace => {
                let discr = &inputs[0];
                let mut output = discr.clone();
                for (lhs, rhs) in inputs[1].lines().filter_map(|x| x.split_once("->")) {
                    if discr.contains(lhs) {
                        output = discr.replace(lhs, rhs);
                        break;
                    }
                }
                self.outputs[0].output_preview.push(output);
            }
            NodeKind::Supervisor => {}
            NodeKind::Filter => {
                if inputs[1] == "keep" {
                    self.outputs[0].output_preview.push(inputs[0].clone());
                }
            }
        }
    }

    fn show(&mut self, ctx: &egui::Context, id: &egui::Id) -> Option<NodeEvent> {
        let mut event = None;
        egui::Window::new(self.name())
            .id(*id)
            .resizable(false)
            .collapsible(false)
            .show(ctx, |ui| {
                if self.kind.is_action() {
                    ui.scope(|ui| {
                        ui.with_layout(egui::Layout::right_to_left(egui::Align::LEFT), |ui| {
                            ui.style_mut().visuals.widgets.inactive.weak_bg_fill =
                                Color32::DARK_RED;
                            if ui.button("remove").clicked() {
                                self.is_closed = true;
                            }
                        });
                    });
                }

                if self.kind == NodeKind::Supervisor {
                    ui.add(egui::Image::new(egui::include_image!(
                        "../assets/maarten_normal_eyes.png"
                    )));
                }

                let field_row_count = self.inputs.len().max(self.outputs.len());
                TableBuilder::new(ui)
                    .id_salt("first table")
                    .column(Column::exact(256.0).resizable(false))
                    .column(Column::remainder().resizable(false))
                    .body(|mut body| {
                        for i in 0..field_row_count {
                            let height = if i < self.inputs.len() && self.inputs[i].multiline {
                                80.0
                            } else {
                                20.0
                            };
                            body.row(height, |mut row| {
                                row.col(|ui| {
                                    if i < self.inputs.len() {
                                        match self.inputs[i].show(ui) {
                                            Some(field_event) => {
                                                event = Some(NodeEvent::FieldEvent {
                                                    inner: field_event,
                                                    node_ref: NodeRef::Input(InputRef {
                                                        window_id: *id,
                                                        field_idx: i,
                                                    }),
                                                })
                                            }
                                            None => {}
                                        }
                                    }
                                });
                                row.col(|ui| {
                                    if i < self.outputs.len() {
                                        if self.outputs[i].non_clickable {
                                            ui.label(&self.outputs[i].name);
                                        } else {
                                            match self.outputs[i].show(ui) {
                                                Some(field_event) => {
                                                    event = Some(NodeEvent::FieldEvent {
                                                        inner: field_event,
                                                        node_ref: NodeRef::Output(OutputRef {
                                                            window_id: *id,
                                                            field_idx: i,
                                                        }),
                                                    })
                                                }
                                                None => {}
                                            }
                                        }
                                    }
                                });
                            });
                        }
                    });

                let mut preview_table = TableBuilder::new(ui).id_salt("second table");
                for _ in &self.outputs {
                    preview_table = preview_table.column(Column::remainder().resizable(false));
                }

                preview_table.body(|mut body| {
                    body.row(20.0, |mut row| {
                        for output in &self.outputs {
                            row.col(|ui| {
                                ui.label(RichText::new(&output.name).strong().underline());
                            });
                        }
                    });

                    for preview_idx in 0..5 {
                        body.row(20.0, |mut row| {
                            for output in &self.outputs {
                                row.col(|ui| {
                                    if let Some(preview_value) =
                                        output.output_preview.get(preview_idx)
                                    {
                                        ui.label(preview_value);
                                    }
                                });
                            }
                        });
                    }
                });
            });

        event
    }

    fn make_action_node(kind: NodeKind) -> Self {
        match kind {
            NodeKind::ProjectFields => panic!(),
            NodeKind::OutputFields => panic!(),
            NodeKind::StripHtml => Node {
                kind,
                inputs: vec![InputField::field_only("input")],
                outputs: vec![OutputField::new("output")],
                is_closed: false,
            },
            NodeKind::Concatenate => Node {
                kind,
                inputs: vec![
                    InputField::field_only("left"),
                    InputField::field_or_value("right"),
                ],
                outputs: vec![OutputField::new("output")],
                is_closed: false,
            },
            NodeKind::SetToValue => Node {
                kind,
                inputs: vec![InputField::value_only("value")],
                outputs: vec![OutputField::new("output")],
                is_closed: false,
            },
            NodeKind::CalculateSum => Node {
                kind,
                inputs: vec![
                    InputField::field_or_value("lhs"),
                    InputField::field_or_value("rhs"),
                ],
                outputs: vec![OutputField::new("output")],
                is_closed: false,
            },
            NodeKind::IfEqual => Node {
                kind,
                inputs: vec![
                    InputField::field_only("input"),
                    InputField::value_only("value"),
                    InputField::field_or_value("when true"),
                    InputField::field_or_value("when false"),
                ],
                outputs: vec![OutputField::new("output")],
                is_closed: false,
            },
            NodeKind::IfContains => Node {
                kind,
                inputs: vec![
                    InputField::field_only("input"),
                    InputField::value_only("value"),
                    InputField::field_or_value("when true"),
                    InputField::field_or_value("when false"),
                ],
                outputs: vec![OutputField::new("output")],
                is_closed: false,
            },
            NodeKind::SearchAndReplace => Node {
                kind,
                inputs: vec![
                    InputField::field_only("input"),
                    InputField::value_only_multiline("mapping"),
                ],
                outputs: vec![OutputField::new("output")],
                is_closed: false,
            },
            NodeKind::Supervisor => Node {
                kind,
                inputs: Vec::new(),
                outputs: Vec::new(),
                is_closed: false,
            },
            NodeKind::Filter => Node {
                kind,
                inputs: vec![
                    InputField::field_only("input"),
                    InputField::field_only("discrimator"),
                ],
                outputs: vec![OutputField::new("output")],
                is_closed: false,
            },
        }
    }

    fn project_fields(fields: &[&str]) -> Self {
        Node {
            kind: NodeKind::ProjectFields,
            inputs: vec![],
            outputs: fields.iter().map(|x| OutputField::new(x)).collect(),
            is_closed: false,
        }
    }

    fn output_fields(fields: &[&str]) -> Self {
        Node {
            kind: NodeKind::OutputFields,
            inputs: fields.iter().map(|x| InputField::field_only(x)).collect(),
            outputs: fields
                .iter()
                .map(|x| OutputField::new_non_clickable(x))
                .collect(),
            is_closed: false,
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
    multiline: bool,
}

impl InputField {
    fn field_only(name: &str) -> Self {
        InputField {
            kind: InputKind::FieldOnly,
            name: name.to_string(),
            position: egui::Pos2::default(),
            multiline: false,
        }
    }

    fn field_or_value(name: &str) -> Self {
        InputField {
            kind: InputKind::FieldOrValue(FieldOrValue::default()),
            name: name.to_string(),
            position: egui::Pos2::default(),
            multiline: false,
        }
    }

    fn value_only(name: &str) -> Self {
        InputField {
            kind: InputKind::ValueOnly {
                value: String::new(),
            },
            name: name.to_string(),
            position: egui::Pos2::default(),
            multiline: false,
        }
    }

    fn value_only_multiline(name: &str) -> Self {
        InputField {
            kind: InputKind::ValueOnly {
                value: String::new(),
            },
            name: name.to_string(),
            position: egui::Pos2::default(),
            multiline: true,
        }
    }

    fn is_currently_for_field(&self) -> bool {
        match &self.kind {
            InputKind::FieldOnly => true,
            InputKind::ValueOnly { .. } => false,
            InputKind::FieldOrValue(field_or_value) => !field_or_value.toggle,
        }
    }

    fn show(&mut self, ui: &mut egui::Ui) -> Option<FieldEvent> {
        let mut event = None;
        ui.with_layout(
            egui::Layout::left_to_right(egui::Align::LEFT),
            |ui| match &mut self.kind {
                InputKind::FieldOnly
                | InputKind::FieldOrValue(FieldOrValue { toggle: false, .. }) => {
                    let response = ui.add(egui::RadioButton::new(false, ""));
                    self.position = response.rect.center();
                    if response.clicked() {
                        event = Some(FieldEvent::LeftClicked)
                    } else if response.secondary_clicked() {
                        event = Some(FieldEvent::RightClicked)
                    }
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
                    let text_edit = if self.multiline {
                        egui::TextEdit::multiline(value)
                    } else {
                        egui::TextEdit::singleline(value)
                    };
                    ui.add(text_edit.clip_text(true).desired_width(128.0));

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

        event
    }
}

pub struct OutputField {
    name: String,
    position: egui::Pos2,
    output_preview: Vec<String>,
    non_clickable: bool,
}

impl OutputField {
    fn new(name: &str) -> Self {
        OutputField {
            name: name.to_string(),
            position: egui::Pos2::default(),
            output_preview: Vec::new(),
            non_clickable: false,
        }
    }

    fn new_non_clickable(name: &str) -> Self {
        // special case for the finalize node
        OutputField {
            name: name.to_string(),
            position: egui::Pos2::default(),
            output_preview: Vec::new(),
            non_clickable: true,
        }
    }

    fn show(&mut self, ui: &mut egui::Ui) -> Option<FieldEvent> {
        let mut event = None;
        ui.with_layout(egui::Layout::right_to_left(egui::Align::LEFT), |ui| {
            let response = ui.add(egui::RadioButton::new(false, ""));
            self.position = response.rect.center();
            if response.clicked() {
                event = Some(FieldEvent::LeftClicked)
            } else if response.secondary_clicked() {
                event = Some(FieldEvent::RightClicked)
            }
            ui.label(&self.name);
        });

        event
    }
}

pub struct RulesApp {
    preview_items: Vec<Item>,
    nodes: HashMap<egui::Id, Node>,
    input_slot: Option<InputRef>,
    output_slot: Option<OutputRef>,
    edges: HashMap<InputRef, OutputRef>,
    inverse_edges: HashMap<OutputRef, Vec<InputRef>>,
    show_dropdown: bool,
    next_id_val: u64,
    initial_node: Option<egui::Id>,
}

impl RulesApp {
    pub fn new() -> Self {
        let mut ret = Self::default();
        ret.add_node(Node::project_fields(&[
            "title",
            "description",
            "price",
            "stock",
            "color",
        ]));
        ret.initial_node = ret.nodes.keys().next().cloned();

        ret.add_node(Node::output_fields(&[
            "amazon_title",
            "description",
            "price",
            "stock",
            "colour",
        ]));

        ret.preview_items.push(Item(HashMap::from_iter([
            ("title".to_string(), "a barrel".to_string()),
            (
                "description".to_string(),
                "<b>a big round barrel</b>".to_string(),
            ),
            ("price".to_string(), "$50.0".to_string()),
            ("stock".to_string(), "-1".to_string()),
            ("color".to_string(), "blue".to_string()),
        ])));

        ret.preview_items.push(Item(HashMap::from_iter([
            ("title".to_string(), "a stool".to_string()),
            ("description".to_string(), "a very comfy stool".to_string()),
            ("price".to_string(), "$30.0".to_string()),
            ("stock".to_string(), "9".to_string()),
            ("color".to_string(), "grey".to_string()),
        ])));

        ret.preview_items.push(Item(HashMap::from_iter([
            ("title".to_string(), "coffee".to_string()),
            (
                "description".to_string(),
                "extremely tasty coffee".to_string(),
            ),
            ("price".to_string(), "$999.0".to_string()),
            ("stock".to_string(), "42".to_string()),
            ("color".to_string(), "porple".to_string()),
        ])));

        ret.preview_items.push(Item(HashMap::from_iter([
            ("title".to_string(), "<i>literally</i> Adrian".to_string()),
            (
                "description".to_string(),
                "A reasonably <img href='huh'> good programmer ".to_string(),
            ),
            ("price".to_string(), "$1.0".to_string()),
            ("stock".to_string(), "1".to_string()),
            ("color".to_string(), "black".to_string()),
        ])));

        ret.preview_items.push(Item(HashMap::from_iter([
            ("title".to_string(), "Falco's first album".to_string()),
            ("description".to_string(), "A stick diss track".to_string()),
            ("price".to_string(), "$69.0".to_string()),
            ("stock".to_string(), "100".to_string()),
            ("color".to_string(), "unknown".to_string()),
        ])));

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

    pub fn remove_closed_nodes(&mut self) {
        self.nodes.retain(|_, n| !n.is_closed);
    }

    pub fn update_edge_state(&mut self) {
        let retain_edges = self
            .edges
            .iter()
            .map(|(lhs, _)| {
                (lhs.clone(), {
                    if let Some(Node {
                        is_closed: true, ..
                    }) = self.nodes.get(&lhs.window_id)
                    {
                        false
                    } else if let Some(input) = self.resolve_input(lhs) {
                        input.is_currently_for_field()
                    } else {
                        false
                    }
                })
            })
            .collect::<HashMap<_, _>>();
        self.edges.retain(|lhs, _| retain_edges[lhs]);

        // more stupid hacks
        self.inverse_edges.clear();
        for (input, output) in &self.edges {
            self.inverse_edges
                .entry(output.clone())
                .or_insert(Vec::new())
                .push(input.clone());
        }
    }

    pub fn resolve_input(&self, node_ref: &InputRef) -> Option<&InputField> {
        self.nodes
            .get(&node_ref.window_id)
            .map(|node| &node.inputs[node_ref.field_idx])
    }

    pub fn resolve_output(&self, node_ref: &OutputRef) -> Option<&OutputField> {
        self.nodes
            .get(&node_ref.window_id)
            .map(|node| &node.outputs[node_ref.field_idx])
    }

    pub fn update_node_previews(&mut self) {
        // seed the project field outputs with the items
        for project_field in self
            .nodes
            .get_mut(&self.initial_node.unwrap())
            .unwrap()
            .outputs
            .iter_mut()
        {
            for item in &self.preview_items {
                project_field
                    .output_preview
                    .push(item.0.get(&project_field.name).unwrap().clone())
            }
        }

        let mut seen: HashSet<egui::Id> = HashSet::default();
        let mut work: VecDeque<egui::Id> = VecDeque::new();
        work.push_back(self.initial_node.unwrap());
        for node_id in self.nodes.keys() {
            work.push_back(*node_id);
        }

        while let Some(node_id) = work.pop_front() {
            if seen.contains(&node_id) {
                continue;
            }
            seen.insert(node_id);
            let node = self.nodes.get_mut(&node_id).unwrap();

            // figure out which outputs to process next
            for (field_idx, _) in node.outputs.iter().enumerate() {
                let output_ref = OutputRef {
                    window_id: node_id,
                    field_idx,
                };

                if let Some(next_ids) = self.inverse_edges.get(&output_ref) {
                    for next_id in next_ids.iter() {
                        work.push_front(next_id.window_id);
                    }
                }
            }

            // ugly hack
            if node.kind != NodeKind::ProjectFields {
                node.clear_output_previews();
            }
            let node = self.nodes.get(&node_id).unwrap();

            let mut all_input_values: Vec<Vec<String>> = Vec::new();

            for i in 0..5 {
                let mut maybe_input_values: Vec<Option<String>> = node
                    .inputs
                    .iter()
                    .enumerate()
                    .map(|(field_idx, input_field)| match &input_field.kind {
                        InputKind::FieldOnly
                        | InputKind::FieldOrValue(FieldOrValue { toggle: false, .. }) => {
                            let input_ref = InputRef {
                                window_id: node_id,
                                field_idx,
                            };
                            self.edges.get(&input_ref).and_then(|output_ref| {
                                self.nodes.get(&output_ref.window_id).and_then(|node| {
                                    node.outputs[output_ref.field_idx]
                                        .output_preview
                                        .get(i)
                                        .cloned()
                                })
                            })
                        }
                        InputKind::ValueOnly { value }
                        | InputKind::FieldOrValue(FieldOrValue {
                            toggle: true,
                            value,
                        }) => Some(value.clone()),
                    })
                    .collect();

                // output fields don't all have to be mapped
                if node.kind == NodeKind::OutputFields {
                    for maybe_val in maybe_input_values.iter_mut() {
                        if maybe_val.is_none() {
                            *maybe_val = Some("".to_string());
                        }
                    }
                }

                let input_values: Vec<String> =
                    maybe_input_values.into_iter().filter_map(|x| x).collect();

                // node is not ready to be previewed
                if input_values.len() != node.inputs.len() {
                    continue;
                }

                all_input_values.push(input_values);
            }

            let node = self.nodes.get_mut(&node_id).unwrap();
            for input_values in all_input_values {
                node.process_preview(&input_values);
            }
        }
    }
}

impl Default for RulesApp {
    fn default() -> Self {
        Self {
            preview_items: Vec::new(),
            nodes: HashMap::new(),
            input_slot: None,
            output_slot: None,
            edges: HashMap::new(),
            inverse_edges: HashMap::new(),
            show_dropdown: false,
            next_id_val: 0,
            initial_node: None,
        }
    }
}

impl eframe::App for RulesApp {
    fn update(&mut self, ctx: &egui::Context, _frame: &mut eframe::Frame) {
        egui_extras::install_image_loaders(ctx);
        ctx.set_pixels_per_point(1.2);
        let mut was_interaction = false;
        for (id, action_node) in self.nodes.iter_mut() {
            if let Some(interaction) = action_node.show(ctx, id) {
                was_interaction = true;
                match interaction {
                    NodeEvent::FieldEvent {
                        inner: FieldEvent::LeftClicked,
                        node_ref,
                    } => match node_ref {
                        NodeRef::Input(input_ref) => self.input_slot = Some(input_ref),
                        NodeRef::Output(output_ref) => self.output_slot = Some(output_ref),
                    },
                    NodeEvent::FieldEvent {
                        inner: FieldEvent::RightClicked,
                        node_ref,
                    } => match node_ref {
                        NodeRef::Input(input_ref) => {
                            self.edges.remove(&input_ref);
                        }
                        NodeRef::Output(_) => {}
                    },
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
                        draw_nice_line(ui, window_pos, mouse_pos);
                    }
                });
        }

        // Draw the lines in an overlay
        egui::Area::new(egui::Id::new("overlay"))
            .order(egui::Order::Foreground)
            .show(ctx, |ui| {
                for (lhs, rhs) in &self.edges {
                    let Some(lhs) = self.resolve_input(lhs) else {
                        continue;
                    };
                    let Some(rhs) = self.resolve_output(rhs) else {
                        continue;
                    };
                    draw_nice_line(ui, lhs.position, rhs.position);
                }
            });

        egui::CentralPanel::default().show(ctx, |ui| {
            let mut set_dropdown_location = false;

            // Check for right-click
            if !was_interaction
                && ui.input(|i| i.pointer.button_down(egui::PointerButton::Secondary))
                && !self.show_dropdown
            {
                self.show_dropdown = true;
                set_dropdown_location = true;
                self.input_slot = None;
                self.output_slot = None;
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
                    for node_kind in NodeKind::iter() {
                        if !node_kind.is_action() {
                            continue;
                        }

                        if ui.button(node_kind.name()).clicked() {
                            let id = self.next_id();
                            self.nodes.insert(id, Node::make_action_node(node_kind));
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

        self.remove_closed_nodes();
        self.update_edge_state();
        self.update_node_previews();
    }
}

fn draw_nice_line(ui: &egui::Ui, from: Pos2, to: Pos2) {
    let painter = ui.painter();

    let d = to - from;
    let p1 = from + egui::Vec2::new(0.2 * d.x, 0.01 * d.y);
    let p2 = to - egui::Vec2::new(0.2 * d.x, 0.01 * d.y);

    let points = [from, p1, p2, to];

    painter.add(egui::Shape::CubicBezier(egui::epaint::CubicBezierShape {
        points,
        closed: false,
        fill: Color32::from_rgba_unmultiplied(0, 0, 0, 0),
        stroke: egui::epaint::PathStroke::new(2.0, Color32::WHITE),
    }));
}
