# reminders.el - Emacs Interface for macOS Reminders

An Emacs package providing a full-featured interface to the macOS `reminders` CLI tool, with enhanced accessibility support for Emacspeak users.

## Features

- **View and manage reminders** from any macOS Reminders list
- **Add, edit, complete, and delete** reminders
- **Sort and filter** by due date, completion status
- **Set priorities** (high/medium/low) and due dates
- **Add notes** to reminders
- **Switch between lists** seamlessly
- **Enhanced Emacspeak support** with custom line reading and auditory icons
- **Evil mode integration** (starts in Emacs state)

## Requirements

- Emacs 27.1 or later
- macOS with the `reminders` CLI tool installed (via Homebrew)
- Optional: Emacspeak for enhanced audio feedback

## Installation

### Install the reminders CLI tool

```bash
brew install reminders
```

### Install the Emacs package

#### Manual Installation

1. Clone this repository:
   ```bash
   git clone https://github.com/robertmeta/reminders-wrap.git
   ```

2. Add to your Emacs configuration:
   ```elisp
   (use-package reminders
     :ensure nil
     :load-path "~/path/to/reminders-wrap"
     :commands (reminders-show reminders-quick-add)
     :init
     (setq reminders-default-list "Reminders")
     :bind (("s-R" . reminders-show)
            ("M-R" . reminders-quick-add)))
   ```

## Usage

### Commands

- `M-x reminders-show` - Open the reminders buffer (or `s-R`)
- `M-x reminders-quick-add` - Quickly add a reminder (or `M-R`)

### Keybindings in reminders-mode

| Key     | Command                          |
|---------|----------------------------------|
| `RET`   | Toggle completion status         |
| `a`     | Add new reminder                 |
| `e`     | Edit reminder title              |
| `d`     | Delete reminder                  |
| `g`     | Refresh view                     |
| `l`     | Switch to different list         |
| `L`     | Show all lists                   |
| `t`     | Toggle showing completed items   |
| `s`     | Sort by due date                 |
| `N`     | Add/edit notes                   |
| `P`     | Set priority                     |
| `D`     | Set due date                     |
| `n/p`   | Next/previous line               |
| `q`     | Quit window                      |

### Display Format

Reminders are displayed in this format:

```
 0: [ ] Fix the thermostat for Guest House
 1: [X] Morning pills (1 week ago)
 2: [!] Dog Nails (in 1 week)
```

Where:
- `[ ]` = not completed, `[X]` = completed
- `[!!!]` = high priority, `[!!]` = medium, `[!]` = low
- Date shown in parentheses (relative format)

## Emacspeak Support

This package includes comprehensive Emacspeak integration:

### Custom Line Reading

When navigating with Emacspeak, lines are read in a cleaner format:
- **Visual**: `0: [ ] Morning pills (1 week ago)`
- **Spoken**: "Morning pills due 1 week ago, not completed, item 0"

The spoken format:
- Starts with the reminder title (most important)
- Includes priority if set ("high priority", "medium priority", "low priority")
- Includes due date in natural language
- States completion status
- Ends with item number (for reference)
- Skips visual noise like brackets

### Auditory Icons

- **Opening reminders**: "open-object" sound
- **Toggling completion**: "select-object" sound
- **Adding reminders**: "item" sound
- **Deleting reminders**: "delete-object" sound

### Voice Personalities

- **Completed items**: Spoken in monotone voice (de-emphasized)
- **Active items**: Spoken in bold voice (emphasized)

## Configuration

### Customization Variables

```elisp
;; Default list to use
(setq reminders-default-list "Reminders")

;; Path to reminders command (if not in PATH)
(setq reminders-command "/opt/homebrew/bin/reminders")

;; Show completed reminders by default
(setq reminders-show-completed nil)
```

### Example Configuration

```elisp
(use-package reminders
  :ensure nil
  :load-path "~/projects/robertmeta/reminders-wrap"
  :commands (reminders-show reminders-quick-add reminders-new-list)
  :init
  (setq reminders-default-list "Todo")
  (setq reminders-show-completed nil)
  :bind (("s-R" . reminders-show)
         ("M-R" . reminders-quick-add)))
```

## Development

### Project Structure

```
reminders-wrap/
├── README.md           # This file
├── reminders.el        # Main package file
└── .gitignore         # Git ignore patterns
```

### Contributing

Contributions are welcome! Please feel free to submit issues or pull requests.

## License

MIT License - see LICENSE file for details.

## Acknowledgments

- Built for the macOS `reminders` CLI tool by Keith Smiley
- Emacspeak integration inspired by T.V. Raman's Emacspeak project
- Created as an accessibility-first interface to macOS Reminders

## See Also

- [reminders CLI](https://github.com/keith/reminders-cli) - The underlying CLI tool
- [Emacspeak](https://github.com/tvraman/emacspeak) - The complete audio desktop
