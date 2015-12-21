package net.ohloh.ohcount4j.scan;

public class BfkScanner extends BaseScanner{

	%%{
		machine brainfuck;
		include common "common.rl";
		
		brainfuck_operator = [+\-<>.,\[\]] @code;
		
		brainfuck_line := |*
    		spaces;
    		newline;
    		brainfuck_operator  => code;
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