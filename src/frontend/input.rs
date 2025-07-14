use eframe::egui;
use winit::{dpi::PhysicalPosition, event::MouseScrollDelta, keyboard::KeyCode};

#[derive(Debug, Clone, Copy)]
pub enum Input {
    LeftClickPressed { shift: bool },
    LeftClickReleased,
    RightClickPressed { shift: bool },
    RightClickReleased,
    MouseMove(f32, f32),
    KeyPress(Key),
    KeyRelease(Key),

    MouseScoll(MouseScrollDelta),

    UnknownInput(UnknownInput),
}

#[derive(Debug, Clone, Copy)]
enum UnknownInput {
    UnknownKeyInput(winit::keyboard::NativeKeyCode),
}

impl TryFrom<winit::event::KeyEvent> for Input {
    type Error = ();

    // TODO: Instead of just using the physical key, it might be better to use other info aswell, for accessability?
    // But maybe that is just fixed with implementing key remapping?
    fn try_from(event: winit::event::KeyEvent) -> Result<Self, Self::Error> {
        let ret = match event.physical_key {
            winit::keyboard::PhysicalKey::Code(key_code) => match event.state {
                winit::event::ElementState::Pressed => Self::KeyPress(key_code.try_into()?),
                winit::event::ElementState::Released => Self::KeyRelease(key_code.try_into()?),
            },
            winit::keyboard::PhysicalKey::Unidentified(native_key_code) => {
                Self::UnknownInput(UnknownInput::UnknownKeyInput(native_key_code))
            },
        };

        Ok(ret)
    }
}

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
                modifiers,
            } => match unit {
                eframe::egui::MouseWheelUnit::Point => Ok(Input::MouseScoll(
                    MouseScrollDelta::PixelDelta(PhysicalPosition {
                        x: delta.x as f64,
                        y: delta.y as f64,
                    }),
                )),
                eframe::egui::MouseWheelUnit::Line => Ok(Input::MouseScoll(
                    MouseScrollDelta::LineDelta(delta.x, delta.y),
                )),
                eframe::egui::MouseWheelUnit::Page => Err(()),
            },

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
    Q,
    P,
    T,
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
}

impl TryFrom<winit::keyboard::KeyCode> for Key {
    type Error = ();

    fn try_from(value: winit::keyboard::KeyCode) -> Result<Self, Self::Error> {
        let ret = match value {
            KeyCode::KeyW => Key::W,
            KeyCode::KeyA => Key::A,
            KeyCode::KeyS => Key::S,
            KeyCode::KeyD => Key::D,
            KeyCode::KeyQ => Key::Q,
            KeyCode::KeyR => Key::R,
            KeyCode::KeyP => Key::P,
            KeyCode::KeyT => Key::T,
            KeyCode::Digit0 => Key::Key0,
            KeyCode::Digit1 => Key::Key1,
            KeyCode::Digit2 => Key::Key2,
            KeyCode::Digit3 => Key::Key3,
            KeyCode::Digit4 => Key::Key4,
            KeyCode::Digit5 => Key::Key5,
            KeyCode::Digit6 => Key::Key6,
            KeyCode::Digit7 => Key::Key7,
            KeyCode::Digit8 => Key::Key8,
            KeyCode::Digit9 => Key::Key9,
            KeyCode::ShiftLeft | KeyCode::ShiftRight => Key::Shift,

            _ => return Err(()),
        };

        Ok(ret)
    }
}

struct EguiInputState {
    key: egui::Key,
    shift: bool,
}

impl TryFrom<EguiInputState> for Key {
    type Error = ();

    fn try_from(value: EguiInputState) -> Result<Self, Self::Error> {
        let ret = match (value.key, value.shift) {
            (egui::Key::W, _) => Key::W,
            (egui::Key::A, _) => Key::A,
            (egui::Key::S, _) => Key::S,
            (egui::Key::D, _) => Key::D,
            (egui::Key::Q, _) => Key::Q,
            (egui::Key::R, false) => Key::R,
            (egui::Key::R, true) => Key::ShiftR,
            (egui::Key::T, _) => Key::T,
            (egui::Key::P, _) => Key::P,
            (egui::Key::Num0, _) => Key::Key0,
            (egui::Key::Num1, _) => Key::Key1,
            (egui::Key::Num2, _) => Key::Key2,
            (egui::Key::Num3, _) => Key::Key3,
            (egui::Key::Num4, _) => Key::Key4,
            (egui::Key::Num5, _) => Key::Key5,
            (egui::Key::Num6, _) => Key::Key6,
            (egui::Key::Num7, _) => Key::Key7,
            (egui::Key::Num8, _) => Key::Key8,
            (egui::Key::Num9, _) => Key::Key9,

            _ => return Err(()),
        };

        Ok(ret)
    }
}
