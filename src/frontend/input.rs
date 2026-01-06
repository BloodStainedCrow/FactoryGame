#[cfg(feature = "client")]
use eframe::egui;

#[derive(Debug, PartialEq, Clone, Copy)]
pub enum Input {
    LeftClickPressed { shift: bool, ctrl: bool },
    LeftClickReleased,
    RightClickPressed { shift: bool },
    RightClickReleased,
    MouseMove(f32, f32),
    KeyPress(Key),
    KeyRelease(Key),

    Copy,

    MouseScoll((f64, f64)),

    UnknownInput,
}

#[cfg(feature = "client")]
impl TryFrom<egui::Event> for Input {
    type Error = ();

    fn try_from(event: egui::Event) -> Result<Self, Self::Error> {
        let ret = match event {
            eframe::egui::Event::Key {
                key,
                physical_key: _,
                pressed,
                repeat: _,
                modifiers,
            } => {
                let key = EguiInputState {
                    key,
                    shift: modifiers.shift,
                    ctrl: modifiers.ctrl,
                };

                if pressed {
                    Ok(Input::KeyPress(key.try_into()?))
                } else {
                    Ok(Input::KeyRelease(key.try_into()?))
                }
            },
            eframe::egui::Event::PointerMoved(pos2) => Ok(Input::MouseMove(pos2.x, pos2.y)),
            eframe::egui::Event::PointerButton {
                pos: _,
                button,
                pressed,
                modifiers,
            } => match (pressed, button) {
                (true, eframe::egui::PointerButton::Primary) => Ok(Input::LeftClickPressed {
                    shift: modifiers.shift,
                    ctrl: modifiers.ctrl,
                }),
                (true, eframe::egui::PointerButton::Secondary) => Ok(Input::RightClickPressed {
                    shift: modifiers.shift,
                }),
                (true, eframe::egui::PointerButton::Middle) => Err(()),
                (true, eframe::egui::PointerButton::Extra1) => Err(()),
                (true, eframe::egui::PointerButton::Extra2) => Err(()),
                (false, eframe::egui::PointerButton::Primary) => Ok(Input::LeftClickReleased),
                (false, eframe::egui::PointerButton::Secondary) => Ok(Input::RightClickReleased),
                (false, eframe::egui::PointerButton::Middle) => Err(()),
                (false, eframe::egui::PointerButton::Extra1) => Err(()),
                (false, eframe::egui::PointerButton::Extra2) => Err(()),
            },
            eframe::egui::Event::MouseWheel {
                unit,
                delta,
                modifiers: _modifiers,
            } => match unit {
                eframe::egui::MouseWheelUnit::Point => {
                    Ok(Input::MouseScoll((delta.x as f64, delta.y as f64)))
                },
                eframe::egui::MouseWheelUnit::Line => {
                    // TODO: This is hardcoded to the default of egui
                    // See InputOptions::default and https://github.com/emilk/egui/issues/461
                    let units_per_line = if cfg!(all(target_arch = "wasm32", target_os = "unknown"))
                    {
                        8.0
                    } else {
                        40.0
                    };

                    Ok(Input::MouseScoll((
                        delta.x as f64 / units_per_line,
                        delta.y as f64 / units_per_line,
                    )))
                },
                eframe::egui::MouseWheelUnit::Page => Err(()),
            },
            egui::Event::Copy => Ok(Self::Copy),

            _ => Err(()),
        };

        ret
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Key {
    Shift,
    W,
    A,
    S,
    D,
    E,
    Q,
    P,
    T,
    M,
    V,
    H,
    Key0,
    Key1,
    Key2,
    Key3,
    Key4,
    Key5,
    Key6,
    Key7,
    Key8,
    Key9,
    R,
    ShiftR,
    Esc,
    Del,
}

#[cfg(feature = "client")]
struct EguiInputState {
    key: egui::Key,
    shift: bool,
    ctrl: bool,
}

#[cfg(feature = "client")]
impl TryFrom<EguiInputState> for Key {
    type Error = ();

    fn try_from(value: EguiInputState) -> Result<Self, Self::Error> {
        let ret = match (value.key, value.shift, value.ctrl) {
            (egui::Key::W, _, false) => Key::W,
            (egui::Key::A, _, false) => Key::A,
            (egui::Key::S, _, false) => Key::S,
            (egui::Key::D, _, false) => Key::D,
            (egui::Key::Q, _, false) => Key::Q,
            (egui::Key::E, _, false) => Key::E,
            (egui::Key::R, false, false) => Key::R,
            (egui::Key::R, true, false) => Key::ShiftR,
            (egui::Key::T, _, false) => Key::T,
            (egui::Key::P, _, false) => Key::P,
            (egui::Key::M, _, false) => Key::M,
            (egui::Key::V, _, false) => Key::V,
            (egui::Key::H, _, false) => Key::H,
            (egui::Key::Num0, _, false) => Key::Key0,
            (egui::Key::Num1, _, false) => Key::Key1,
            (egui::Key::Num2, _, false) => Key::Key2,
            (egui::Key::Num3, _, false) => Key::Key3,
            (egui::Key::Num4, _, false) => Key::Key4,
            (egui::Key::Num5, _, false) => Key::Key5,
            (egui::Key::Num6, _, false) => Key::Key6,
            (egui::Key::Num7, _, false) => Key::Key7,
            (egui::Key::Num8, _, false) => Key::Key8,
            (egui::Key::Num9, _, false) => Key::Key9,
            (egui::Key::Escape, _, false) => Key::Esc,
            (egui::Key::Delete, _, false) => Key::Del,

            _ => return Err(()),
        };

        Ok(ret)
    }
}
