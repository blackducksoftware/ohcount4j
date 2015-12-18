package net.ohloh.ohcount4j.scan;

public class BfppScanner extends BaseScanner{

  	%%{
  		machine bfpp;
		include common "common.rl";
		
  		bfpp_operator = [+\-<>.,\[\]\%\!\#\^\:\;] @code;

  		bfpp_line_comment = ('=') @comment nonnewline*;

  		bfpp_include = '@include(' @code (
      		newline
      		|
      		spaces
      		|
      		(any - newline) @code
   		)* :>> ')';

  		bfpp_line := |*
    		spaces;
    		newline;
    		bfpp_line_comment;
    		bfpp_include;
    		bfpp_operator;
    		^space => comment;
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