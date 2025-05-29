use std::sync::Mutex;
pub use termcolor::Color;
use termcolor::{ColorChoice, StandardStream};
use unicode_segmentation::UnicodeSegmentation;
use unicode_width::UnicodeWidthStr;

pub static STDOUT: std::sync::LazyLock<Mutex<StandardStream>> =
    std::sync::LazyLock::new(|| Mutex::new(StandardStream::stdout(ColorChoice::Auto)));

pub static IS_UTF8: std::sync::LazyLock<bool> = std::sync::LazyLock::new(|| {
    utf8_supported::utf8_supported() == utf8_supported::Utf8Support::UTF8
});

/// Estimate the width of the terminal. Falls back to 79 if the width cannot be
/// determined.
pub fn term_width() -> usize {
    termsize::get().map(|s| (s.cols - 1) as usize).unwrap_or(79)
}

pub fn compute_rule_string(message: &str, max_width: usize) -> String {
    if message.width() <= max_width {
        message.to_string()
    } else {
        let mut chars = message.graphemes(true);

        let mut start = String::with_capacity(max_width / 2);
        let mut end = String::with_capacity(max_width / 2);
        let ellipsis = "…";

        loop {
            if let Some(grapheme) = chars.next() {
                let prev_len = start.len();
                start.push_str(grapheme);
                if start.width() + end.width() + ellipsis.width() > max_width {
                    start.truncate(prev_len);
                    break;
                }
            } else {
                break;
            }
            if let Some(grapheme) = chars.next_back() {
                let prev_len = end.len();
                end.insert_str(0, grapheme);
                if start.width() + end.width() + ellipsis.width() > max_width {
                    end.drain(0..(end.len() - prev_len));
                    break;
                }
            } else {
                break;
            }
        }

        let s = format!("{}{}{}", start, ellipsis, end);
        debug_assert!(s.width() <= max_width);
        s
    }
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
    () => {
        {
            use termcolor::{WriteColor, ColorSpec};
            use std::io::Write;
            let mut stdout = $crate::term::STDOUT.lock().unwrap();
            _ = stdout.set_color(&ColorSpec::new());
            _ = writeln!(&mut stdout);
        }
    };
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
            #[allow(unused_mut)]
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

/// Print a rule of dashes, optionally with an embedded message.
///
/// ```nocompile
/// -[messsage]----------------- ...
/// ```
#[macro_export]
macro_rules! cprintln_rule {
    () => {
        let is_utf8 = *$crate::term::IS_UTF8;

        if is_utf8 {
            $crate::cprintln!(
                dimmed = true,
                "{:─>count$}",
                "",
                count = $crate::term::term_width() - 1
            );
        } else {
            $crate::cprintln!(
                dimmed = true,
                "{:->count$}",
                "",
                count = $crate::term::term_width() - 1
            );
        }
    };
    ($(fg=$fg:expr,)? $(bg=$bg:expr,)? $(bold=$bold:expr,)? $(dimmed=$dimmed:expr,)? $literal:literal $($arg:tt)*) => {
        use ::unicode_width::UnicodeWidthStr;

        let message = format!($literal $($arg)*);
        const UNTOUCHABLE: usize = 1 + 8; // --[ ... ]--

        // If there's not enough space, just skip printing the extra rule overlay.
        if $crate::term::term_width() > UNTOUCHABLE {
            let max_width = $crate::term::term_width() - UNTOUCHABLE;
            let message = $crate::term::compute_rule_string(&message, max_width);
            let message_width = message.width();

            let is_utf8 = *$crate::term::IS_UTF8;

            if is_utf8 {
                $crate::cprint!(dimmed = true, "{:─>count$}", "", count = max_width - message_width);
            } else {
                $crate::cprint!(dimmed = true, "{:->count$}", "", count = max_width - message_width);
            }

            if is_utf8 {
                $crate::cprint!(dimmed = true, "┨ ");
            } else {
                $crate::cprint!(dimmed = true, "[ ");
            }
            $crate::cprint!($(fg = $fg,)? $(bg = $bg,)? $(bold = $bold,)? $(dimmed = $dimmed,)? "{message}");
            if is_utf8 {
                $crate::cprint!(dimmed = true, " ┣");
            } else {
                $crate::cprint!(dimmed = true, " ]");
            }

            if is_utf8 {
                $crate::cprint!(dimmed = true, "━━");
            } else {
                $crate::cprint!(dimmed = true, "--");
            }
            $crate::cprintln!();
        } else {
            $crate::cprintln_rule!();
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_compute_rule_string() {
        assert_eq!(compute_rule_string("Hello, world!", 10), "Hello…rld!");
        assert_eq!(compute_rule_string("Hello, world!", 11), "Hello…orld!");
        assert_eq!(compute_rule_string("Hello, world!", 12), "Hello,…orld!");
        assert_eq!(compute_rule_string("Hello, world!", 13), "Hello, world!");
        assert_eq!(compute_rule_string("Hello, world!", 14), "Hello, world!");
    }
}
