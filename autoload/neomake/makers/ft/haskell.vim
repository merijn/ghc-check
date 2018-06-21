function! neomake#makers#ft#haskell#ghccheck() abort
    let params = {
        \ 'exe': 'ghc-check',
        \ 'args': ['check'],
        \ 'errorformat':
            \ '%-Z %#,'.
            \ '%W%f:%l:%v: Warning: %m,'.
            \ '%W%f:%l:%v: Warning:,'.
            \ '%E%f:%l:%v: %m,'.
            \ '%E%>%f:%l:%v:,'.
            \ '%+C  %#%m,'.
            \ '%W%>%f:%l:%v:,'.
            \ '%+C  %#%tarning: %m,'
        \ }
    return params
endfunction
