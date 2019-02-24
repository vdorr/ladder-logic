
function! InsertFunctionBlock( name, ... )

  let x = col(".")
  let y = line(".")

  let save_pos = getpos(".")

"  execute "%!LadderInsertBlock.hs " . " " y . " " x . " " . a:name
  execute "%!LadderInsertBlock.hs " . " " y . " " x . " " . a:name join(a:000)
  call setpos(".", save_pos)

endfunction

command -nargs=* FB :call InsertFunctionBlock(<f-args>)
