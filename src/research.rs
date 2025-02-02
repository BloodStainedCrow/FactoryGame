#[derive(Debug, Clone, Default, serde::Deserialize, serde::Serialize)]
pub struct Technology {
    pub name: String,
    pub cost: [u16; 6],
}

#[derive(Debug, Default, serde::Deserialize, serde::Serialize)]
pub struct TechState {
    pub current_technology: Technology,
}

impl TechState {
    pub fn apply_progress(&mut self, tech_progress: u16) {
        // TODO:
    }
}
