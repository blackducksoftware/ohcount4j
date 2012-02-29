%%{

machine c;

include common "common.rl";

c_line_comment = '//' nonnewline* @comment;

c_block_comment_begin = '/*' @comment;
c_block_comment_end = '*/' @comment;

c_block_comment := |*
	c_block_comment_end => { fret; };
	spaces;
	newline;
	(any - newline) => comment;
*|;

}%%
