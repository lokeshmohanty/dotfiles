" " denite file search (c-p uses gitignore, c-o looks at everything)
" map <C-P> :DeniteProjectDir -buffer-name=git -direction=top file/rec/git<CR>
" map <C-O> :DeniteProjectDir -buffer-name=files -direction=top file/rec<CR>
"
" " -u flag to unrestrict (see ag docs)
" call denite#custom#var('file/rec', 'command',
" \ ['ag', '--follow', '--nocolor', '--nogroup', '-u', '-g', ''])
"
" call denite#custom#alias('source', 'file/rec/git', 'file/rec')
" call denite#custom#var('file/rec/git', 'command',
" \ ['ag', '--follow', '--nocolor', '--nogroup', '-g', ''])
"
" " denite content search
" map <leader>a :DeniteProjectDir -buffer-name=grep -default-action=quickfix grep:::!<CR>
"
" call denite#custom#source(
" \ 'grep', 'matchers', ['matcher_regexp'])
"
" " use ag for content search
" call denite#custom#var('grep', 'command', ['ag'])
" call denite#custom#var('grep', 'default_opts',
"     \ ['-i', '--vimgrep'])
" call denite#custom#var('grep', 'recursive_opts', [])
" call denite#custom#var('grep', 'pattern_opt', [])
" call denite#custom#var('grep', 'separator', ['--'])
" call denite#custom#var('grep', 'final_opts', [])

nnoremap <C-p> :Denite -direction=topleft -start-filter file/rec<CR>
nnoremap <Leader>/ :Denite -direction=topleft -start-filter grep<CR>
nnoremap <Leader>s :Denite -direction=topleft -start-filter buffer<CR>

autocmd FileType denite call s:denite_my_settings()
function! s:denite_my_settings() abort
  nnoremap <silent><buffer><expr> <CR> denite#do_map('do_action')
  nnoremap <silent><buffer><expr> p denite#do_map('do_action', 'preview')
  nnoremap <silent><buffer><expr> q denite#do_map('quit')
  nnoremap <silent><buffer><expr> i denite#do_map('open_filter_buffer')
  nnoremap <silent><buffer><expr> t denite#do_map('toggle_select').'j'
endfunction

autocmd FileType denite-filter call s:denite_filter_my_settings()
function! s:denite_filter_my_settings() abort
  inoremap <silent><buffer><expr> <CR> denite#do_map('do_action')

  " quit _everything_ instead of just closing filter window with denite_filter_quit
  imap <silent><buffer><expr> <C-[> denite#do_map('quit')
  imap <silent><buffer><expr> <Esc> denite#do_map('quit')

  " C-j/C-k to navigate denite buffer from filter
  inoremap <silent><buffer> <C-j> <Esc><C-w>p:call cursor(line('.')+1,0)<CR><C-w>pA
  inoremap <silent><buffer> <C-k> <Esc><C-w>p:call cursor(line('.')-1,0)<CR><C-w>pA
endfunction
