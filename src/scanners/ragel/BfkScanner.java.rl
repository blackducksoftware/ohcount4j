package net.ohloh.ohcount4j.scan;

public class BfkScanner extends BaseScanner{

	%%{
		machine bfk;
		include common "common.rl";
		
		bfk_operator = [+\-<>.,\[\]] @code;
		
		bfk_line := |*
    		spaces;
    		newline;
    		bfk_operator  => code;
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