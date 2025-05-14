#[derive(Debug, Clone, Default, serde::Deserialize, serde::Serialize)]
pub struct Technology {
    pub id: u16, //65k Technologies should suffice :)
}

pub type ResearchProgress = u16;

#[derive(Debug, Clone, Default, serde::Deserialize, serde::Serialize)]
pub struct TechState {
    pub current_technology: Option<Technology>,
    // current_tech_mod_lookup: (),
}

impl TechState {
    pub fn apply_progress(&mut self, tech_progress: u16) {
        // TODO:
        if tech_progress > 0 {
            dbg!(tech_progress);
        }
    }
}
