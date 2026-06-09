pub enum EnergySource {
    // TODO(BSC): Do I want to support burner entities? And if so how?
    NoEnergy,
    ElectricEnergy { drain: Watt, active_energy: Watt },
}

pub struct Watt(u64);
pub struct Joule(u64);
