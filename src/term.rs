use std::sync::Mutex;
pub use termcolor::Color;
use termcolor::{ColorChoice, StandardStream};

pub static STDOUT: std::sync::LazyLock<Mutex<StandardStream>> =
    std::sync::LazyLock::new(|| Mutex::new(StandardStream::stdout(ColorChoice::Auto)));

/// We need to filter out onig panic messages.
pub fn ensure_panic_hook() {
    static PANIC_HOOK_LOCK: std::sync::OnceLock<()> = std::sync::OnceLock::new();
    PANIC_HOOK_LOCK.get_or_init(|| {
        let old_hook = std::panic::take_hook();
        std::panic::set_hook(Box::new(move |panic_info| {
            let payload = panic_info.payload();
            if let Some(s) = payload.downcast_ref::<String>() {
                if s.contains("Onig: Regex search error:") {
                    return;
                }
            }
            old_hook(panic_info);
        }));
    });
}

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
