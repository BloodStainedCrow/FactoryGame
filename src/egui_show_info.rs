use get_size::GetSize;

pub trait ShowInfo: Sized {
    fn show_info<E: InfoExtractor<Self, Info>, Info>(ui: &mut egui::Ui);
}

pub trait InfoExtractor<T, Info> {
    fn extract_info(&self) -> Info;
}

pub trait EguiDisplayable {
    fn show(&self, ui: &mut egui::Ui);
}

struct RamSize(usize);

impl EguiDisplayable for RamSize {
    fn show(&self, ui: &mut egui::Ui) {
        let value = match self.0 {
            v @ 0..1024 => format!("{} KB", v),
            v @ 1024..1_048_576 => format!("{} MB", v / 1024),
            v @ 1_048_576.. => format!("{} GB", v / 1_048_576),
        };
        ui.label(value);
    }
}

struct RAMSizeExtractor;

impl<T: GetSize> InfoExtractor<T, RamSize> for RAMSizeExtractor {
    fn extract_info(&self) -> RamSize {
        RamSize(self.get_size())
    }
}
