if &compatible
    set nocompatible
endif

""""" " dein Plugin Manager
set runtimepath+=~/.local/share/dein/repos/github.com/Shougo/dein.vim
if dein#load_state(expand('~/.local/share/dein'))
    call dein#begin(expand('~/.local/share/dein'))

    " Plugin Manager
    call dein#add('Shougo/dein.vim')
    if !has('nvim')
        call dein#add('roxma/nvim-yarp')
        call dein#add('roxma/vim-hug-neovim-rpc')
    endif

    " " Autocomplete
    call dein#add('Shougo/deoplete.nvim', { 'build': ':UpdateRemotePlugins' })       " Requires :UpdateRemotePlugins
    call dein#add('Shougo/denite.nvim', { 'build': ':UpdateRemotePlugins' })         " Unites all interfaces, a bit like fuzzy finder but more
    " call dein#add('neoclide/coc.nvim')


    call dein#add('itchyny/lightline.vim')
    call dein#add('tomtom/tcomment_vim')                " gc{movement}, gcc{single line}, gcp{paragraph}
    call dein#add('bling/vim-bufferline')               " :help bufferline
    " call dein#add('mattn/emmet-vim')                  " helpful for HTML, CSS. Go to github site for tutorial
    " call dein#add('mattn/gist-vim')
    call dein#add('terryma/vim-multiple-cursors')

    " Plugin for file directory management
    " call dein#add('Shougo/defx.nvim')
    " call dein#add('scrooloose/nerdtree')
    " call dein#add('Xuyuanp/nerdtree-git-plugin')

    " call dein#add('tpope/vim-surround')
    " call dein#add('mattn/webapi-vim')

    " For git tools within vim
    " call dein#add('tpope/vim-fugitive')     " :Gblame, :Gbrowse {using git tools with vim}
    " call dein#add('shumphrey/fugitive-gitlab.vim')
    " call dein#add('tpope/vim-rhubarb')
    " call dein#add('tommcdo/vim-fubitive')

    " For folding
    " call dein#add('nelstrom/vim-markdown-folding')
    " call dein#add('LucHermitte/VimFold4C')
    " call dein#add('LucHermitte/lh-vim-lib')
    " call dein#add('sgeb/vim-diff-fold')

    " For live preview of latex
    " call dein#add('xuhdev/vim-latex-live-preview')

    " For live preview of markdown
    " call dein#add('suan/vim-instant-markdown')

    " For markdown syntax highlighting
    " call dein#add('tpope/vim-markdown')

    " Colorschemes
    " call dein#add('morhetz/gruvbox')
    " call dein#add('lifepillar/vim-solarized8')
    " call dein#add('dikiaap/minimalist')
    " call dein#add('owickstrom/vim-colors-paramount')
    call dein#add('nightsense/carbonized')

    call dein#add('kristijanhusak/vim-hybrid-material') 
    " call dein#add('vim-syntastic/syntastic')
    call dein#add('ryanoasis/vim-devicons')

    " Javascript Linter
    call dein#add('dense-analysis/ale')
    call dein#add('leafgarland/typescript-vim')
    call dein#add('mhartington/nvim-typescript', {'build': './install.sh'})   " Requires :UpdateRemotePlugins
    call dein#add('HerringtonDarkholme/yats.vim')   " syntax file required by mharington/nvim-typescript

    " Repeat plugin commands
    " call dein#add('tpope/vim-repeat')
    " call dein#add('tpope/vim-unimpaired')

    " Plugins to try
    " call dein#add('junegunn/fzf')
    " call dein#add('SirVer/ultisnips') "for creating custom snippets
    if dein#check_install()
        call dein#install()
        let pluginsExist=1
    endif

    call dein#end()
    call dein#save_state()
endif

" =========================Learning[Start]================================"
"
" ----------Normal Mode-------------
" *, # => search for the word under the cursor
" g*, g# => partial search
" ^U, ^D => scroll half page up/down/up
" ^F, ^B => scroll full page forward/backward
" 
" ^O, ^I => move backward/forward cursor positions
" ``, '' => move to last cursor position
" m[char] => mark
" `[char] => jump to mark
" ', ", [, ] => special marks
" zt, zz, zb => puts the cursor line at the top/mid/bottom
"
" daw => delete the word the cursor is on(along with the following space)[delete a word]
" das => delete the object the cursor is on[delete a sentence]
" dis => delete the object the cursor is on[is => text object]
" diw => delete current word 
" ~ => toggle case
" g~ => toggle case mode [~:line, rest same as other movement shortcuts]
" gU => Upper case mode [U:line, rest same as other movement shortcuts]
" gu => lower case mode [u:line, rest same as other movement shortcuts]
"
" ZZ => :x ; ZQ => :q!
"
" ----------Command Mode-------------
" :set noincsearch => disables display of matches while typing for search
" :set nowrapsearch => stops search at the end of the file
" :set nohlsearch => disables hilighting the matches for search
" :nohlsearch => clears all the hilighting done by search
" :set ignorecase => search ignores case
"
"
" :set number => show line numbers
" :set ruler => show cursor position at bottom right corner
" =========================Learning[End]================================"

" =========================Plugins[Start]================================"
" -----------------Lightline----------------
let g:lightline = {
      \ 'colorscheme': 'carbonized_dark',
      \ }
" let g:lightline.separator = {
"       \ 'left': "▶",
"       \ 'right': "◀",
"       \ }
" let g:lightline.subseparator = {
"       \ 'left': "〉",
"       \ 'right': "〈",
"       \ }
" --------------------------------------------

" -----------------Deoplete----------------
" Enable deoplete at startup
let g:deoplete#enable_at_startup = 1
inoremap <expr><tab> pumvisible() ? "\<c-n>" : "\<tab>"

" Disable deoplete for denite buffer
autocmd FileType denite-filter
      \   call deoplete#custom#buffer_option('auto_complete', v:false)
" --------------------------------------------

"-----------------  Denite -----------------------
" call denite#custom#option('default', {
"       \ 'prompt': '❯'
"       \ })
"
" call denite#custom#var('file/rec', 'command',
"       \ ['fd', '-H', '--full-path'])
" call denite#custom#var('grep', 'command', ['rg'])
" call denite#custom#var('grep', 'default_opts',
"       \ ['--hidden', '--vimgrep', '--smart-case'])
" call denite#custom#var('grep', 'recursive_opts', [])
" call denite#custom#var('grep', 'pattern_opt', ['--regexp'])
" call denite#custom#var('grep', 'separator', ['--'])
" call denite#custom#var('grep', 'final_opts', [])
"
" autocmd FileType denite call s:denite_settings()
"
" function! s:denite_settings() abort
"   nnoremap <silent><buffer><expr> <CR>
"         \ denite#do_map('do_action')
"   nnoremap <silent><buffer><expr> <C-v>
"         \ denite#do_map('do_action', 'vsplit')
"   nnoremap <silent><buffer><expr> d
"         \ denite#do_map('do_action', 'delete')
"   nnoremap <silent><buffer><expr> p
"         \ denite#do_map('do_action', 'preview')
"   nnoremap <silent><buffer><expr> <Esc>
"         \ denite#do_map('quit')
"   nnoremap <silent><buffer><expr> q
"         \ denite#do_map('quit')
"   nnoremap <silent><buffer><expr> i
"         \ denite#do_map('open_filter_buffer')
" endfunction
"
" autocmd FileType denite-filter call s:denite_filter_settings()
"
" function! s:denite_filter_settings() abort
"   nmap <silent><buffer> <Esc> <Plug>(denite_filter_quit)
" endfunction
"
" nnoremap <C-p> :<C-u>Denite file/rec -start-filter<CR>
" nnoremap <leader>s :<C-u>Denite buffer<CR>
" nnoremap <leader>8 :<C-u>DeniteCursorWord grep:.<CR>
" nnoremap <leader>/ :<C-u>Denite grep:.<CR>
" nnoremap <leader><Space>/ :<C-u>DeniteBufferDir grep:.<CR>
" nnoremap <leader>d :<C-u>DeniteBufferDir file/rec -start-filter<CR>
" nnoremap <leader>r :<C-u>Denite -resume -cursor-pos=+1<CR>
" nnoremap <leader><C-r> :<C-u>Denite register:.<CR>
" nnoremap <leader>g :<C-u>Denite gitstatus<CR>
"
" hi link deniteMatchedChar Special

" --------------------------------------------

" ----------------Settings for syntastic------
" set statusline+=%#warningmsg#
" set statusline+=%{SyntasticStatuslineFlag()}
" set statusline+=%*

" let g:syntastic_always_populate_loc_list = 1
" let g:syntastic_auto_loc_list = 1
" let g:syntastic_check_on_open = 1
" let g:syntastic_check_on_wq = 0
" --------------------------------------------

"-----------  Vim-instant-markdown -----------
" To prevent update of display in realtime of vim-instant-markdown
let g:instant_markdown_slow = 1

" To prevent autostart of markdown preview. to start use
" :InstantMarkdownPreview
let g:instant_markdown_autostart = 0
" --------------------------------------------

"-----------  Javascript Plugins  -----------
let g:ale_linters = {
      \ 'javascript': ['eslint'],
      \ 'typescript': ['tsserver', 'tslint'],
      \ 'vue': ['eslint']
      \ }

let g:ale_fixers = {
       \ 'javascript': ['eslint'],
       \ 'typescript': ['prettier'],
       \ 'vue': ['eslint'],
       \ 'scss': ['prettier'],
       \ 'html': ['prettier']
       \ }
let g:ale_fix_on_save = 1

" autocmd FileType typescript setlocal formatprg=prettier\ --parser\ typescript


" " Decrease updatetime to a smaller value
" autocmd Filetype tex setl updatetime=1
" let g:livepreview_previewer = 'evince'

" --------------------------------------------

"-----------  Plulgin -----------
" --------------------------------------------

" =========================Plugins[End]================================"



" =========================System Settings[Start]================================"

set shell=/bin/sh
filetype plugin indent on         " Turn on filetype plugins
syntax enable                     " Enable syntax highlight
set termguicolors				          " Enable true-color support
set autoindent				            " Set autoindent
set smartindent				            " Set smartindent
set laststatus=2				          " Always show window status
set ruler				                  " Show line and column number
set wildmenu				              " Show the nice autocomplete menu
set encoding=utf-8				        " Set enciding to utf-8
set autoread				              " Reload unchanged files automatically
set autochdir                     " Automatically change directory to file location

" Enable persistent undo
set undodir=~/.nvimundo/
set undofile

" Enable live preview while substitution
set inccommand=split

set lazyredraw				            " Enable lazyredraw
set mouse=a				                " Enable mouse for navigation
set wildmode=longest,full				  " Set full autocompletion
set smartcase				              " Don't ignore case

" Auto-center on search result
noremap n nzz
noremap N Nzz

set title				                  " Set window title
set number				                " Show line numbers
set showcmd				                " Show unfinished command

" Trigger autoread when changing buffers or coming back to vim in terminal.
au FocusGained,BufEnter * :silent! !

" Highlight search results
" set hlsearch				              
set nohlsearch

set guicursor=                    " To prevent unknown symbol in ex mode
set expandtab				              " Tabs are made up of spaces
set tabstop=2				              " Show existing tab with 2 spaces width

" When shiftwidth is 0, tabstop value is used
" this defines '>'
set shiftwidth=0

set t_Co=256
let g:solarized_termcolors=256
set noswapfile
set incsearch
" set guifont=InconsolataGo\ Nerd\ Font\ Mono\ Regular\ 15
set splitright      " Open new split panes to right and bottom
set splitbelow      " Open new split panes to right and bottom
set scrolloff=8     " Start scrolling when we're 8 lines away from margins
set sidescrolloff=15
set sidescroll=1

" Set the colorscheme
" colorscheme gruvbox
" let g:gruvbox_italic = 1
" let g:gruvbox_bold = 0
" let g:gruvbox_contrast_dark = 'hard'

" colorscheme solarized8
" colorscheme minimalist
" colorscheme paramount
colorscheme carbonized-dark
" set background=dark
" let g:enable_bold_font = 1

" Visually hide the annoying tilde signs
" hi! EndOfBuffer ctermbg=bg ctermfg=bg guibg=bg guifg=bg

" ------------- Js linting -------------------
let g:ale_fixers = {
  \'javascript': ['eslint']
  \}
" --------------------------------------------

" ------------- Netrw -------------------
" let g:netrw_liststyle = 0         " Choose the directory view (hit i to cycle through the options when netrw is open)
let g:netrw_banner = 0            " To remove the banner (hit I to remove temporarily)
" let g:netrw_browse_split = 0      " Open file in the previous window
let g:netrw_winsize = 25
" let g:netrw_altv = 1      " To split towards right

" To open netrw along with vim
" augroup ProjectDrawer
"   autocmd!
"   autocmd VimEnter * :Vexplore
" augroup END
" --------------------------------------------


" =========================System Settings[End]================================"


" Toggle relative numbering and set to absolute on focus loss and insert mode
" set rnu
" function! ToggleNumbersOn()
"     set nu!
"     set rnu
" endfunction
" function! ToggleRelativeOn()
"     set rnu!
"     set nu
" endfunction
" autocmd FocusLost * call ToggleRelativeOn()
" autocmd FocusGained * call ToggleRelativeOn()
" autocmd InsertEnter * call ToggleRelativeOn()
" autocmd InsertLeave * call ToggleRelativeOn()



" =================================================================
"                              MAPPINGS
" =================================================================

" Define <semi-colon> as leader
let mapleader=";"

nmap <Leader>f :Lexplore<CR>
inoremap {<CR> {}<ESC>i<CR><ESC>O
nnoremap <M-b> :buffers<CR>:buffer<Space>
" nnoremap <Leader>t :Terminal<CR>

nnoremap <Leader>> :vertical resize +5<CR>
nnoremap <Leader>< :vertical resize -5<CR>
nnoremap <Leader>+ :resize +5<CR>
nnoremap <Leader>- :resize -5<CR>
" nnoremap <silent> <Leader>+ :exe 'resize ' . (winheight(0) * 3/2)<CR>
" nnoremap <silent> <Leader>- :exe 'resize ' . (winheight(0) * 2/3)<CR>
" nnoremap <silent> <Leader>> :exe 'vertical resize ' . (winwidth(0) * 3/2)<CR>
" nnoremap <silent> <Leader>< :exe 'vertical resize ' . (winwidth(0) * 2/3)<CR>

" Enable faster navigation
nmap <silent> <Leader>k :wincmd k<CR>
nmap <silent> <Leader>j :wincmd j<CR>
nmap <silent> <Leader>h :wincmd h<CR>
nmap <silent> <Leader>l :wincmd l<CR>

" Force saving files that require root permission
cnoremap w!! w !sudo tee > /dev/null %

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
    " autocmd BufRead,BufNewFile *.md setlocal textwidth=100

    " Automatically wrap at 100 characters and spell check git commit messages
    " autocmd FileType gitcommit setlocal textwidth=100
    autocmd FileType gitcommit setlocal spell

    " Allow stylesheets to autocomplete hyphenated words
    autocmd FileType css,scss,sass,less setlocal iskeyword+=-
augroup END

