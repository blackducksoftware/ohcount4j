package net.ohloh.ohcount4j.scan;

// Yields only blank lines and code lines.
public class GenericCodeScanner extends BaseScanner{
	%%{
	machine generic_code;
	include common "common.rl";

	generic_code_line := |*
		spaces;
		newline;
		(any - newline) => code;
	*|;
	}%%

	%% write data;

	@Override
	public void doScan(){
		%% write init;
		init();
		%% write exec;
	}

}
