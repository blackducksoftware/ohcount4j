package com.blackducksoftware.ohcount4j.scan;

public class AugeasScanner extends BaseScanner{

	int nest_count = 0;

	%%{
		machine augeas;
		include common "common.rl";
		
		# Augeas supports nested comments.
		action augeas_comment_nc_res { nest_count = 0; }
		action augeas_comment_nc_inc { nest_count++; }
		action augeas_comment_nc_dec { nest_count--; }
		
		augeas_nested_block_comment = '(*' >augeas_comment_nc_res @comment (
			newline
			|
			spaces
			|
			'(*' @augeas_comment_nc_inc @comment
			|
			'*)' @augeas_comment_nc_dec @comment
			|
			(any - newline) @comment
		)* :>> ('*)' when { nest_count == 0 }) @comment;
		augeas_comment = augeas_nested_block_comment;
	
		augeas_line := |*
			spaces;
			augeas_comment;
			newline;
			(any - newline) => code;
		*|;
	}%%

	%% write data;
	
	@Override
	public void doScan(){
		// variables and data is set up in BaseScanner
		%% write init;
		init();
		%% write exec;
	}
	
}
