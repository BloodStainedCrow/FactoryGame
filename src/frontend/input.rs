use winit::event::MouseScrollDelta;

#[derive(Debug, Clone, Copy)]
pub enum Input {
    LeftClickPressed,
    LeftClickReleased,
    RightClickPressed,
    RightClickReleased,
    MouseMove(f32, f32),
    KeyPress(winit::keyboard::KeyCode),
    KeyRelease(winit::keyboard::KeyCode),

    MouseScoll(MouseScrollDelta),

    UnknownInput(UnknownInput),
}

#[derive(Debug, Clone, Copy)]
enum UnknownInput {
    UnknownKeyInput(winit::keyboard::NativeKeyCode),
}

impl From<winit::event::KeyEvent> for Input {
    // TODO: Instead of just using the physical key, it might be better to use other info aswell, for accessebility?
    // But maybe that is just fixed with implementing key remapping?
    fn from(event: winit::event::KeyEvent) -> Self {
        match event.physical_key {
            winit::keyboard::PhysicalKey::Code(key_code) => match event.state {
                winit::event::ElementState::Pressed => Self::KeyPress(key_code),
                winit::event::ElementState::Released => Self::KeyRelease(key_code),
            },
            winit::keyboard::PhysicalKey::Unidentified(native_key_code) => {
                Self::UnknownInput(UnknownInput::UnknownKeyInput(native_key_code))
            },
        }
    }
}
