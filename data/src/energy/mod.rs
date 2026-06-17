#[derive(Debug, Clone, Copy, serde::Serialize, serde::Deserialize)]
pub enum EnergySource {
    // TODO(BSC): Do I want to support burner entities? And if so how?
    NoEnergy,
    ElectricEnergy { drain: Watt, active_energy: Watt },
}

#[derive(Debug, Clone, Copy, serde::Serialize, serde::Deserialize)]
pub struct Watt(u64);

#[derive(Debug, Clone, Copy, serde::Serialize, serde::Deserialize)]
pub struct Joule(u64);
