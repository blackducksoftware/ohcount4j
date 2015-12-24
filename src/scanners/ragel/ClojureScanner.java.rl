package net.ohloh.ohcount4j.scan;

public class ClojureScanner extends BaseScanner{

  %%{
    machine clojure;

    include common "common.rl";

    clojure_line_comment = ';' @comment nonnewline* @comment;

    clojure_line := |*
    	clojure_line_comment;
    	spaces;
    	newline;
    	string_literal;
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
