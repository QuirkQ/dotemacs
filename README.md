# 🚀 Quint's Emacs Configuration

Welcome to my turbo-charged Emacs setup with **F19 super shortcuts**!

## 💡 About F19 Key Mapping

I've mapped my **Caps Lock** key to **F19** for lightning-fast development shortcuts. This gives me easy access to all essential development tools without complex key combinations.

## 🎯 F19 Shortcuts Cheatsheet

> **Tip**: Press `F19 ?` to see all available shortcuts in Emacs!

### 📁 Project & File Navigation

| Shortcut | Command | Description |
|----------|---------|-------------|
| `F19 p` | `project-find-file` | Find file in current project |
| `F19 t` | `treemacs` | Toggle file tree sidebar |
| `F19 b` | `ivy-switch-buffer` | Switch between open buffers |
| `F19 k` | `kill-this-buffer` | Close current buffer |
| `F19 w` | `save-buffer` | Save current file |
| `F19 ←` | `previous-buffer` | Go to previous buffer |
| `F19 →` | `next-buffer` | Go to next buffer |
| `F19 d` | `counsel-git` | Find git-tracked files |
| `F19 f` | `counsel-git-grep` | Search in git repository |

### 🔄 Git Operations

| Shortcut | Command | Description |
|----------|---------|-------------|
| `F19 g s` | `magit-status` | Open git status (Magit) |
| `F19 g c` | `magit-commit` | Create git commit |
| `F19 g p` | `magit-push` | Push to remote repository |
| `F19 g l` | `magit-log-all` | View git commit history |
| `F19 g b` | `magit-blame` | Show git blame for current file |
| `F19 g f` | `magit-pull` | Fetch/pull from remote |

### 🤖 AI/GPT Assistance

| Shortcut | Command | Description |
|----------|---------|-------------|
| `F19 a g` | `aidermacs-start` | Start Aidermacs (Aider) in the project |
| `F19 a s` | `aidermacs-ask-about-region` | Ask AI about the selected region |
| `F19 a f` | `aidermacs-add-file` | Add current file to Aider context |
| `F19 a b` | `aidermacs-add-buffer` | Add current buffer to Aider context |
| `F19 a r` | `my/aidermacs-drop-current-file` | Remove current file from Aider context |
| `F19 a R` | `my/aidermacs-drop-all` | Remove all files from Aider context |
| `F19 a k` | `aidermacs-quit` | Quit Aidermacs session |

### 🛠️ Development Tools

| Shortcut | Command | Description |
|----------|---------|-------------|
| `F19 c c` | `compile` | Compile current project |
| `F19 c r` | `run tests` | Run RSpec/Ruby tests |
| `F19 c d` | `docker` | Docker management interface |
| `F19 c f` | `format buffer` | Format with StandardRB |
| `F19 c l` | `flycheck-list-errors` | Show linting errors |
| `F19 c t` | `vterm` | Open terminal |

### 🪟 Window & Buffer Management

| Shortcut | Command | Description |
|----------|---------|-------------|
| `F19 o` | `other-window` | Switch to other window |
| `F19 1` | `delete-other-windows` | Make current window fill frame |
| `F19 2` | `split-window-below` | Split window horizontally |
| `F19 3` | `split-window-right` | Split window vertically |
| `F19 0` | `delete-window` | Close current window |
| `F19 =` | `balance-windows` | Balance all window sizes |

### 📼 Macros & Automation

| Shortcut | Command | Description |
|----------|---------|-------------|
| `F19 r` | `kmacro-start-macro` | Start recording macro |
| `F19 e` | `kmacro-end-macro` | Stop recording macro |
| `F19 SPC` | `kmacro-call-macro` | Execute last macro |
| `F19 m` | `kmacro-name-last-macro` | Name the last macro |

### ⚡ Quick Actions

| Shortcut | Command | Description |
|----------|---------|-------------|
| `F19 ;` | `comment-or-uncomment-region` | Toggle comments |
| `F19 u` | `undo` | Undo last action |
| `F19 /` | `swiper` | Search in current buffer |
| `F19 ?` | `which-key-show-top-level` | Show available shortcuts |
| `F19 i` | `imenu` | Jump to function/class |
| `F19 j` | `avy-goto-char` | Jump to any character |
| `F19 l` | `goto-line` | Go to specific line number |
| `F19 x` | `execute-extended-command` | Alternative to M-x |
| `F19 q` | `keyboard-quit` | Cancel current operation |

### 💎 Ruby/Rails Specific

| Shortcut | Command | Description |
|----------|---------|-------------|
| `F19 R r` | `robe-jump` | Jump to Ruby definition |
| `F19 R d` | `robe-doc` | Show Ruby documentation |
| `F19 R s` | `robe-start` | Start Ruby completion server |
| `F19 R c` | `rails console` | Open Rails console |

### 🎪 Special Functions

| Shortcut | Command | Description |
|----------|---------|-------------|
| `F19 ESC` | `keyboard-escape-quit` | Ultimate escape/cancel |
| `F19 F19` | `execute-extended-command` | Double-tap for M-x |

## 🎨 Theme & Appearance

- **Theme**: Doom Moonlight (dark, easy on the eyes)
- **Font**: Menlo (clean macOS programming font)
- **Modeline**: Doom Modeline with environment info
- **Icons**: Nerd Icons throughout the interface

## 📦 Key Packages Used

### Core Productivity
- **Ivy/Counsel/Swiper**: Fuzzy finding and search
- **Magit**: Git integration
- **Treemacs**: File tree sidebar
- **Company**: Auto-completion
- **Flycheck**: Real-time linting

### Ruby Development
- **Robe**: Ruby completion and navigation
- **Tree-sitter**: Modern syntax highlighting
- **StandardRB**: Ruby code formatting

### AI Integration
- **Aidermacs**: Emacs-native Aider integration for AI pair programming
- Supports OpenAI-compatible providers via OpenRouter

### Terminal & Docker
- **VTerm**: Fast terminal emulator
- **Multi-VTerm**: Multiple terminal management
- **Docker.el**: Docker container management

## 🚀 Quick Start Tips

1. **Press `F19 ?`** to see all available shortcuts
2. **Use `F19 p`** to quickly find files in your project
3. **Try `F19 a g`** to start Aidermacs; use `F19 a s` to ask about a region
4. **Use `F19 g s`** for git operations
5. **Press `F19 t`** to toggle the file tree

## 🔧 Setup Instructions

1. Map your Caps Lock to F19 at the system level
2. Clone this configuration to `~/.emacs.d/`
3. Install Emacs 29+ for tree-sitter support
4. Run Emacs - packages will install automatically
5. Enjoy your supercharged development environment!

## 💡 Pro Tips

- **Macros**: Record repetitive tasks with `F19 r`, stop with `F19 e`, replay with `F19 SPC`
- **AI Workflows**:
  - `F19 a g` to start Aidermacs in the project
  - `F19 a f` / `F19 a b` to add current file/buffer to context
  - `F19 a s` to ask about the selected region
  - `F19 a r` / `F19 a R` to drop current/all files from context
- **Project Navigation**: Use `F19 d` for git files, `F19 p` for all project files
- **Window Management**: `F19 2` and `F19 3` for splits, `F19 1` to focus
- **Ruby Development**: `F19 c f` formats with StandardRB, `F19 c r` runs tests

---

*Happy coding! 🎉*
