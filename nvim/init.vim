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
    " colourschemes
    call dein#add('morhetz/gruvbox')
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
filetype plugin indent on
syntax enable
set termguicolors   " Not sure what it actually does
set guicursor=      " To prevent unknown symbol in ex mode
set number          " To make line numbers appear 
set tabstop=4       " Defining a tablength to be equal to 4 spaces
set shiftwidth=0    " When shitwidth is zero, tabstop value is used
set expandtab       " Tabs are made up of spaces
set showcmd         " Display incomplete command
set noswapfile
set nohlsearch
set incsearch
set foldmethod=expr
" set guifont=InconsolataGo\ Nerd\ Font\ Mono\ Regular\ 15

set autoread        " Reload files changed outside vim
 
" Trigger autoread when changing buffers or coming back to vim in terminal.
au FocusGained,BufEnter * :silent! !
set splitright      " Open new split panes to right and bottom
set splitbelow      " Open new split panes to right and bottom
set scrolloff=8     " Start scrolling when we're 8 lines away from margins
set sidescrolloff=15
set sidescroll=1

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
 

 
" For colorscheme
"
set background=dark
colorscheme gruvbox
let g:enable_bold_font = 1
" let g:enable_italic_font = 1


" Enable live preview while substitution
set inccommand=split

" Enable deoplete at startup
let g:deoplete#enable_at_startup = 1

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


" Mapings
nmap <C-d> :NERDTreeToggle<CR>
inoremap {<CR> <CR>{}<ESC>i<CR><ESC>O
 
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
