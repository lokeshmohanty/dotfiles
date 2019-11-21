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

    " call dein#add('Shougo/vimproc.vim', {'build' : 'make'})

    " " Autocomplete
    call dein#add('Shougo/deoplete.nvim', { 'build': ':UpdateRemotePlugins' })       " Requires :UpdateRemotePlugins
    call dein#add('Shougo/denite.nvim', { 'build': ':UpdateRemotePlugins' })         " Unites all interfaces, a bit like fuzzy finder but more
    " call dein#add('neoclide/coc.nvim')
    " call dein#add('Quramy/tsuquyomi')


    call dein#add('itchyny/lightline.vim')
    call dein#add('tomtom/tcomment_vim')                " gc{movement}, gcc{single line}, gcp{paragraph}
    call dein#add('bling/vim-bufferline')               " :help bufferline
    " call dein#add('mattn/emmet-vim')                  " helpful for HTML, CSS. Go to github site for tutorial
    " call dein#add('mattn/gist-vim')
    call dein#add('terryma/vim-multiple-cursors')

    " Plugin for file directory management
    call dein#add('Shougo/defx.nvim', { 'build': ':UpdateRemotePlugins' })
    call dein#add('kristijanhusak/defx-git')
    call dein#add('kristijanhusak/defx-icons')

    " call dein#add('tpope/vim-surround')
    " call dein#add('mattn/webapi-vim')

    " For git tools within vim
    call dein#add('tpope/vim-fugitive')     " :Gblame, :Gbrowse {using git tools with vim}
    " call dein#add('shumphrey/fugitive-gitlab.vim')
    " call dein#add('tpope/vim-rhubarb')
    " call dein#add('tommcdo/vim-fubitive')

    " Pairs of handy bracket mappings
    call dein#add('tpope/vim-unimpaired')

    " Repeat plugin commands
    call dein#add('tpope/vim-repeat')


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
    " call dein#add('leafgarland/typescript-vim')
    " call dein#add('mhartington/nvim-typescript', {'build': './install.sh'})   " Requires :UpdateRemotePlugins, :TSDoc 
    " call dein#add('HerringtonDarkholme/yats.vim')   " syntax file required by mharington/nvim-typescript

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

" To remove uneeded repositories
" call map(dein#check_clean(), "delete(v:val, 'rf')")

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
"
" CTRL-w H/J/K/L => moves the current window to far left/down/up/right
"
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
" 
" :set noscrollbind => to prevent scrollbind in diff mode
"
" :set modifiable => file cannot be edited
" :set write => file can be edited
"
" :saveas <filename> => writes current buffer to filename and edits that file
" :file <filename> => same as saveas except that `filename` is not saved
"
" :close => closes the current window without exiting vim
" :only => closes all windows except the current one
" :split => opens the current file in a new window
" :new => opens an emplty file in a new window
" :[range]split <file> => opens file in a new window of height [range]
" :qall /:wall /:wqall => quit/write all windows
"
" :diffsplit <file> => enter diff mode from within vim
" :diffpatch <file.diff> => patches the current file without saving
"
" " Buffers
" :bwipe => wipes the buffer along with memory 
" :bdelete => deletes the buffer and not the memory
" :ls, :buffers => shows a list of buffers
" :buffer <buff-name> => opens the buffer in current window
" :sb <buff-name> => opens the buffer in a split
" :vertical sb <buff-name> => opens the buffer in a vertical split
"
"
" <C-r><C-f> => recall the filename the cursor is on
"
"
"
"
"
"
"
" :mksession path/to/my-session-file.vim
" nvim -S path/to/my-session-file.vim
"
"
"
" vim -R file (or) view file => read-only file, cannot be written but can be editted
" vim -M file => non modifiable file, cannot be edited 
"
" vim -o ...file => open all files in splits
" vim -O ...file => open all files in vertical splits
" vim -d file1 file2 => diff mode
"
"
" =========================Learning[End]================================"


" =================================================================
"                              MAPPINGS
" =================================================================

" Define <space> as leader
nnoremap <Space> <Nop>
let mapleader=" "

inoremap {<CR> {}<ESC>i<CR><ESC>O
nnoremap <M-b> :buffers<CR>:buffer<Space>
nnoremap <M-v> :buffers<CR>:vsplit<Space>#
nnoremap <M-s> :buffers<CR>:split<Space>#
" nnoremap <Leader>t :Terminal<CR>

nnoremap <Leader>> :vertical resize +5<CR>        " [range]<CTRL-w>+        , [range]<CTRL-w>_ => sets height to range
nnoremap <Leader>< :vertical resize -5<CR>        " [range]<CTRL-w>-
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

" -----Terminal 
tnoremap <ESC> <C-\><C-n>
nnoremap <Leader>q :bwipe!<CR>
" nnoremap <Leader>vt :vsplit|terminal<CR>
" nnoremap <Leader>ht :split|terminal<CR>
nnoremap <Leader>tv :vsplit term://bash<CR>
nnoremap <Leader>th :split term://bash<CR>
nnoremap <Leader>tt :tabnew term://bash<CR>
nnoremap <Leader>te :edit term://bash<CR>

" Force saving files that require root permission
cnoremap w!! w !sudo tee > /dev/null %


" =========================Plugins[Start]================================"

" -----------------Deoplete----------------
" Enable deoplete at startup
let g:deoplete#enable_at_startup = 1
inoremap <expr><tab> pumvisible() ? "\<c-n>" : "\<tab>"

" Disable deoplete for denite buffer
autocmd FileType denite-filter
      \   call deoplete#custom#buffer_option('auto_complete', v:false)
" --------------------------------------------


"-------------------------------  Fugitive ------------------------------
nnoremap <Leader>gs :Gstatus<CR>
nnoremap <Leader>gp :Gpush<CR>

" -----------------------------------------------------------------------------

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
" let g:ale_fix_on_save = 1

" autocmd FileType typescript setlocal formatprg=prettier\ --parser\ typescript
" --------------------------------------------


" ------------------------------- Tsuquyomi -------------------------------------
"
" <C-x><C-o> {insert mode} => completion
" <C-]> => got to definition, alternative: `:TsuquyomiDefinition` or `:TsuDefinition`
" :TsuGoBack => go to previous jump
" <C-t> => last location <C-]> was typed
" <C-^> => references, alternative: `:TsuReferences`
" :TsuSearch {keyword} => get list of locations containing the keyword
"
" :TsuGeterr, :TsuGeterrProject, :TsuQuickFix
"
" :TsuquyomiReloadProject
"
"

" To prevent popup menu for omni-completion
" autocmd FileType typescript setlocal completeopt-=menu

" To show signature in popup menu, slows completion
" let g:tsuquyomi_completion_detail = 1

" To show signature in preview window
" autocmd FileType typescript setlocal completeopt+=menu,preview

let g:tsuquyomi_shortest_import_path = 1

" To rename a symbol, 2nd one is for comments as well
autocmd FileType typescript nmap <buffer> <Leader>e <Plug>(TsuquyomiRenameSymbol)
autocmd FileType typescript nmap <buffer> <Leader>E <Plug>(TsuquyomiRenameSymbolC)

" work around for jump to definition issue
let g:tsuquyomi_use_local_typescript = 0

" To veiw definition as a tooltip
" autocmd FileType typescript nmap <buffer> <Leader>q : <C-u>echo tsuquyomi#hint()<CR> 
" ----------------------------------------------------------------------------------


"-------------------------------  Defx --------------------------------------------

" [c => goes to next file with git status
" ]c => goes to previous file with git status

let g:loaded_netrwPlugin = 1     " disable netrw.vim

" nnoremap <Leader>f :Defx
"       \ -split=vertical
"       \ -winwidth=30 -direction=topleft
"       \ -columns=git:icons:indent:filename:type
"       \ `expand('%:p:h')` -search=`expand('%:p')`<CR>
nnoremap <Leader>f :Defx -split=vertical -winwidth=30 -direction=topleft -columns=git:icons:indent:filename:type `expand('%:p:h')` -search=`expand('%:p')`<CR>

augroup defxConfig
  autocmd!
  autocmd FileType defx call s:defx_my_settings()
augroup END

function! s:defx_my_settings() abort
	  nnoremap <silent><buffer><expr> <CR> defx#do_action('drop')
	  nnoremap <silent><buffer><expr> c defx#do_action('copy')
	  nnoremap <silent><buffer><expr> m defx#do_action('move')
	  nnoremap <silent><buffer><expr> p defx#do_action('paste')
	  nnoremap <silent><buffer><expr> l defx#do_action('open')
	  nnoremap <silent><buffer><expr> E defx#do_action('open', 'vsplit')
	  nnoremap <silent><buffer><expr> P defx#do_action('open', 'pedit')
	  nnoremap <silent><buffer><expr> o defx#do_action('open_or_close_tree')
	  nnoremap <silent><buffer><expr> K defx#do_action('new_directory')
	  nnoremap <silent><buffer><expr> N defx#do_action('new_file')
	  nnoremap <silent><buffer><expr> M defx#do_action('new_multiple_files')
	  nnoremap <silent><buffer><expr> C
	  \ defx#do_action('toggle_columns',
	  \                'mark:indent:icon:filename:type:size:time')
	  nnoremap <silent><buffer><expr> S defx#do_action('toggle_sort', 'time')
	  nnoremap <silent><buffer><expr> d defx#do_action('remove')
	  nnoremap <silent><buffer><expr> r defx#do_action('rename')
	  nnoremap <silent><buffer><expr> ! defx#do_action('execute_command')
	  nnoremap <silent><buffer><expr> x defx#do_action('execute_system')
	  nnoremap <silent><buffer><expr> yy defx#do_action('yank_path')
	  nnoremap <silent><buffer><expr> . defx#do_action('toggle_ignored_files')
	  nnoremap <silent><buffer><expr> ; defx#do_action('repeat')
	  nnoremap <silent><buffer><expr> h defx#do_action('cd', ['..'])
	  nnoremap <silent><buffer><expr> ~ defx#do_action('cd')
	  nnoremap <silent><buffer><expr> q defx#do_action('quit')
	  nnoremap <silent><buffer><expr> <Space> defx#do_action('toggle_select') . 'j'
	  nnoremap <silent><buffer><expr> * defx#do_action('toggle_select_all')
	  nnoremap <silent><buffer><expr> j line('.') == line('$') ? 'gg' : 'j'
	  nnoremap <silent><buffer><expr> k line('.') == 1 ? 'G' : 'k'
	  nnoremap <silent><buffer><expr> <C-l> defx#do_action('redraw')
	  nnoremap <silent><buffer><expr> <C-g> defx#do_action('print')
	  nnoremap <silent><buffer><expr> cd defx#do_action('change_vim_cwd')
  " nnoremap <silent><buffer><expr> <CR> defx#do_action('open', 'wincmd w \| drop')
  " nnoremap <silent><buffer><expr> M defx#do_action('new_multiple_files')
  " nnoremap <silent><buffer><expr> ~ defx#do_action('cd', [getcwd()])
  " nnoremap <silent><buffer><expr> C defx#do_action('toggle_columns', 'mark:filename:type:size:time')
  " nnoremap <silent><buffer><expr> S defx#do_action('toggle_sort', 'time')
  " nnoremap <silent><buffer><expr> e defx#do_action('call', 'OpenRanger')
  " nnoremap <silent><buffer><expr> e defx#do_action('call', 'DefxExternalExplorer')
endfunction
" ----------------------------------------------------------------------------------------


"-----------  Texlive  ----------------------
" " Decrease updatetime to a smaller value
" autocmd Filetype tex setl updatetime=1
" let g:livepreview_previewer = 'evince'
" -------------------------------------------------------


"-------------------------------  Plulgin ------------------------------
" -----------------------------------------------------------------------------

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

" ------------- Netrw -------------------
" let g:netrw_liststyle = 3         " Choose the directory view (hit i to cycle through the options when netrw is open)
" let g:netrw_banner = 0            " To remove the banner (hit I to remove temporarily)
" let g:netrw_winsize = 25
" let g:netrw_altv = 1      " To split towards right

" ++++ Mappings
" nmap <Leader>f :Lexplore<CR>

" ++++ Autocommands
" Wipe the irritating files created by netrw
" autocmd FileType netrw setl bufhidden=wipe

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
"                              AUTOCOMMANDS
" =================================================================

" Trigger autoread when changing buffers or coming back to vim in terminal.
au FocusGained,BufEnter * :silent! !

" Automatically go to insert mode in terminal
au BufEnter * if &buftype == 'terminal' | :startinsert | endif

" Below is to fix issues with the ABOVE mappings in quickfix window
autocmd CmdwinEnter * nnoremap <CR> <CR>
autocmd BufReadPost quickfix nnoremap <CR> <CR>

" =================================================================
" =================================================================
" =================================================================
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

