#[derive(Debug, Clone, Default, serde::Deserialize, serde::Serialize)]
pub struct Technology {
    pub name: String,
    pub cost: [u16; 6],
}

pub type ResearchProgress = u16;

#[derive(Debug, Clone, Default, serde::Deserialize, serde::Serialize)]
pub struct TechState {
    pub current_technology: Technology,
    // current_tech_mod_lookup: (),
}

impl TechState {
    pub fn apply_progress(&mut self, tech_progress: u16) {
        // TODO:
    }
}
