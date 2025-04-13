use std::sync::Mutex;
pub use termcolor::Color;
use termcolor::{ColorChoice, StandardStream};

pub static STDOUT: std::sync::LazyLock<Mutex<StandardStream>> =
    std::sync::LazyLock::new(|| Mutex::new(StandardStream::stdout(ColorChoice::Auto)));

#[macro_export]
macro_rules! println {
    ($($arg:tt)*) => {
        {
            use std::io::Write;
            _ = writeln!(&mut $crate::term::STDOUT.lock().unwrap(), $($arg)*);
        }
    };
}

#[macro_export]
macro_rules! cprintln {
    ($(fg=$fg:expr,)? $(bg=$bg:expr,)? $(bold=$bold:expr,)? $(dimmed=$dimmed:expr,)? $literal:literal $($arg:tt)*) => {
        {
            use std::io::Write;
            $crate::cprint!($(fg=$fg,)? $(bg=$bg,)? $(bold=$bold,)? $(dimmed=$dimmed,)? $literal $($arg)*);
            let mut stdout = $crate::term::STDOUT.lock().unwrap();
            _ = writeln!(&mut stdout);
        }
    };
}

#[macro_export]
macro_rules! cprint {
    ($(fg=$fg:expr,)? $(bg=$bg:expr,)? $(bold=$bold:expr,)? $(dimmed=$dimmed:expr,)? $literal:literal $($arg:tt)*) => {
        {
            use std::io::Write;
            use termcolor::{WriteColor, ColorSpec};

            let mut stdout = $crate::term::STDOUT.lock().unwrap();
            let mut color = ColorSpec::new();
            $(
                color.set_bg(Some($bg));
            )?
            $(
                color.set_fg(Some($fg));
            )?
            $(
                color.set_bold($bold);
            )?
            $(
                color.set_dimmed($dimmed);
            )?
            _ = stdout.set_color(&color);
            _ = write!(&mut stdout, $literal $($arg)*);
            _ = stdout.set_color(&ColorSpec::new());
        }
    };
}
