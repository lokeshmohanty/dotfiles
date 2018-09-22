if &compatible
    set nocompatible
endif

 """"" " dein Plugin Manager
 set runtimepath+=~/.local/share/dein/repos/github.com/Shougo/dein.vim
 if dein#load_state(expand('~/.local/share/dein'))
     call dein#begin(expand('~/.local/share/dein'))

     call dein#add('Shougo/dein.vim')
     call dein#add('Shougo/deoplete.nvim')
     if !has('nvim')
         call dein#add('roxma/nvim-yarp')
         call dein#add('roxma/vim-hug-neovim-rpc')
     endif

     call dein#add('tpope/vim-fugitive')                 " :Gblame, :Gbrowse {using git tools with vim}
     call dein#add('itchyny/lightline.vim')              " lighter alternative to airline
     call dein#add('tomtom/tcomment_vim')                " gc{movement}, gcc{single line}, gcp{paragraph}
     call dein#add('bling/vim-bufferline')               " :help bufferline
     call dein#add('mattn/emmet-vim')                    " helpful for HTML, CSS. Go to github site for tutorial
     call dein#add('mattn/gist-vim')
     call dein#add('terryma/vim-multiple-cursors')
     " call dein#add('junegunn/fzf')
     call dein#add('scrooloose/nerdtree')                " :NERDTree {file directory}
     call dein#add('tpope/vim-surround')                 " surround a text or replace the surround {helpful for html, css}
     call dein#add('Xuyuanp/nerdtree-git-plugin')
     call dein#add('mattn/webapi-vim')
     call dein#add('shumphrey/fugitive-gitlab.vim')      " extend fugitive.vim to support Gitlab urls
     call dein#add('tpope/vim-rhubarb')                  " extends fugitive.vim to support Github urls
     call dein#add('tommcdo/vim-fubitive')               " extend fugitive.vim to support Bitbucket urls
    call dein#add('nelstrom/vim-markdown-folding')      " folding for markdown
    call dein#add('morhetz/gruvbox')                    " colorscheme
    call dein#add('lifepillar/vim-solarized8')          " colorscheme
    call dein#add('kristijanhusak/vim-hybrid-material') 
    call dein#add('vim-syntastic/syntastic')            " automatically check for syntax errors in a file
    call dein#add('ryanoasis/vim-devicons')
    " To be added later
    " call dein#add('tpope/vim-repeat')                   " it remaps '.' to be used by plugins

     if dein#check_install()
         call dein#install()
         let pluginsExist=1
     endif

     call dein#end()
     call dein#save_state()
 endif


" system settings
" Turn on filetype plugins
filetype plugin indent on

" Enable syntax highlight
syntax enable

" Enable true-color support
set termguicolors

" Set autoindent
set autoindent

" Set smartindent
set smartindent

" Always show window status
set laststatus=2

" Show line and column number
set ruler

" Show the nice autocomplete menu
set wildmenu

" Set enciding to utf-8
set encoding=utf-8

" Reload unchanged files automatically
set autoread

" Enable persistent undo
set undodir=~/.nvimundo/
set undofile

" Enable lazyredraw
set lazyredraw

" Enable mouse for navigation
set mouse=a

" Set full autocompletion
set wildmode=longest,full

" Don't ignore case
set smartcase

" Auto-center on search result
noremap n nzz
noremap N Nzz

" Set window title
set title

" Show line numbers
set number

" Show unfinished command
set showcmd

" Trigger autoread when changing buffers or coming back to vim in terminal.
au FocusGained,BufEnter * :silent! !

" Highlight search results
" set hlsearch
set nohlsearch

" To prevent unknown symbol in ex mode
set guicursor=      

" Tabs are made up of spaces
set expandtab       

" Show existing tab with 4 spaces width
set tabstop=4

" When shiftwidth is 0, tabstop value is used
" this defines '>'
set shiftwidth=0

" Set the background
set background=dark

set noswapfile
set incsearch
set foldmethod=expr
" set guifont=InconsolataGo\ Nerd\ Font\ Mono\ Regular\ 15
set splitright      " Open new split panes to right and bottom
set splitbelow      " Open new split panes to right and bottom
set scrolloff=8     " Start scrolling when we're 8 lines away from margins
set sidescrolloff=15
set sidescroll=1


" Set the colorscheme
" colorscheme gruvbox
colorscheme solarized8
let g:enable_bold_font = 1

" Visually hide the annoying tilde signs
hi! EndOfBuffer ctermbg=bg ctermfg=bg guibg=bg guifg=bg

" Start NERDTreeTabs on GUI if dir selected
let g:nerdtree_tabs_open_on_console_startup=2
let g:nerdtree_tabs_smart_startup_focus=2
let g:nerdtree_tabs_focus_on_files=1

" Show hidden files
let NERDTreeShowHidden=1



" Toggle relative numbering and set to absolute on focus loss and insert mode
set rnu
function! ToggleNumbersOn()
    set nu!
    set rnu
endfunction
function! ToggleRelativeOn()
    set rnu!
    set nu
endfunction
autocmd FocusLost * call ToggleRelativeOn()
autocmd FocusGained * call ToggleRelativeOn()
autocmd InsertEnter * call ToggleRelativeOn()
autocmd InsertLeave * call ToggleRelativeOn()
 

 


" Enable live preview while substitution
set inccommand=split

" Enable deoplete at startup
let g:deoplete#enable_at_startup = 1
inoremap <expr><tab> pumvisible() ? "\<c-n>" : "\<tab>"

" Close vim when the only window left is a NERDTree
autocmd bufenter * if (winnr("$") == 1 && exists("b:NERDTree") && b:NERDTree.isTabTree()) | q | endif

" Settings for syntastic
" set statusline+=%#warningmsg#
" set statusline+=%{SyntasticStatuslineFlag()}
" set statusline+=%*

" let g:syntastic_always_populate_loc_list = 1
" let g:syntastic_auto_loc_list = 1
" let g:syntastic_check_on_open = 1
" let g:syntastic_check_on_wq = 0

" =================================================================
"                              MAPPINGS
" =================================================================

" Define <Space> as leader
let mapleader=" "

" Disable <Space> as a single key
map <Space> <nop>

nmap <C-d> :NERDTreeToggle<CR>
inoremap {<CR> {}<ESC>i<CR><ESC>O
nnoremap <M-b> :buffers<CR>:buffer<Space>

" Enable quick closing and saving
nnoremap <Leader>q :q<CR>
nnoremap <Leader>w :w<CR>
nnoremap <Leader>x :x<CR>

" Enable faster navigation
nmap <silent> <Leader>k :wincmd k<CR>
nmap <silent> <Leader>j :wincmd j<CR>
nmap <silent> <Leader>h :wincmd h<CR>
nmap <silent> <Leader>l :wincmd l<CR>

" Disable Arrow keys in Escape mode
map <up> <nop>
map <down> <nop>
map <left> <nop>
map <right> <nop>

" Disable Arrow keys in Insert mode
imap <up> <nop>
imap <down> <nop>
imap <left> <nop>
imap <right> <nop>

" Disable Arrow keys in Visual mode
vmap <up> <nop>
vmap <down> <nop>
vmap <left> <nop>
vmap <right> <nop>

" Force saving files that require root permission
cnoremap w!! w !sudo tee > /dev/null %
 
"Use enter to create new lines w/o entering insert mode
nnoremap <CR> o<Esc>
"Below is to fix issues with the ABOVE mappings in quickfix window
autocmd CmdwinEnter * nnoremap <CR> <CR>
autocmd BufReadPost quickfix nnoremap <CR> <CR>


"" AUTOCOMMANDS

" Save whenever switching windows or leaving vim. This is useful when running
" the tests inside vim without having to save all files first.
au FocusLost,WinLeave * :silent! wa


" automatically rebalance windows on vim resize
autocmd VimResized * :wincmd =
 

"update dir to current file
autocmd BufEnter * silent! cd %:p:h

" When editing a file, always jump tr the last known cursor position.
" Don't do it for commit messages, when the position is invalid, or when
" inside an event handler (happens when dropping a file on gvim).
autocmd BufReadPost *
    \ if &ft != 'gitcommit' && line("'\"") > 0 && line("'\"") <= line("$") |
    \   exe "normal g`\"" |
    \ endif

 
augroup vimrcEx
  autocmd!

  " When editing a file, always jump to the last known cursor position.
  " Don't do it for commit messages, when the position is invalid, or when
  " inside an event handler (happens when dropping a file on gvim).

  " Set syntax highlighting for specific file types
  autocmd BufRead,BufNewFile *.md set filetype=markdown

  " autocmd BufRead *.jsx set ft=jsx.html
  " autocmd BufNewFile *.jsx set ft=jsx.html

  " Enable spellchecking for Markdown
  autocmd FileType markdown setlocal spell

  " Automatically wrap at 100 characters for Markdown
  autocmd BufRead,BufNewFile *.md setlocal textwidth=100

  " Automatically wrap at 100 characters and spell check git commit messages
  autocmd FileType gitcommit setlocal textwidth=100
  autocmd FileType gitcommit setlocal spell

  " Allow stylesheets to autocomplete hyphenated words
  autocmd FileType css,scss,sass,less setlocal iskeyword+=-
augroup END
