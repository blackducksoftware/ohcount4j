package net.ohloh.ohcount4j.scan;

public class BlitzMaxScanner extends BaseScanner{

	%%{
		machine blitzmax;
		include common "common.rl";
		
		blitzmax_line_comment = '\'' @comment nonnewline*;
  		blitzmax_rem_block_comment = /rem/i @comment (
      		newline
      		|
      		spaces
      		|
      		(any - newline) @comment
    	)* :>> (/end rem/i | /endrem/i);

  		blitzmax_comment = blitzmax_line_comment | blitzmax_rem_block_comment;
		
		blitzmax_line := |*
    		spaces;
    		newline;
    		blitzmax_comment;
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