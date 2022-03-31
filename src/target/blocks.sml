signature INST = sig
    type t   (* The type of the instruction *)
    val isJumpLike   : t -> bool
    val isTarget     : t -> bool
end

exception Jump_And_Target

functor BasicBlocks (I : INST) = struct
    structure Inst = I                     (* expose the instruction module as well*)
    type block = I.t list
    fun block_help ([],b,bl) = bl@[b]
      | block_help ((x::xs),block1,block_lis) = if( (not (I.isJumpLike x) ) andalso (not (I.isTarget x )) ) then
                                                    block_help(xs,x::block1,block_lis)
                                                else if(I.isJumpLike x) then
                                                    block_help(xs,[],block_lis@[List.rev(x::block1)]) 
                                                else if(I.isTarget x) then
                                                    block_help(xs,[x],block_lis@[List.rev(block1)])
                                                else 
                                                    raise Jump_And_Target
                                                
    fun basicBlocks (instr) =   block_help(instr,[],[[]])
end