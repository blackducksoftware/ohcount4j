package net.ohloh.ohcount4j.scan;

import net.ohloh.ohcount4j.Language;

public class PythonScanner extends BaseScanner{

  %%{
    machine python;
    include common "common.rl";
    include c "c.rl";
    
    python_line_comment = c_line_comment | ('#' @comment nonnewline* @comment);
    
    python_sq_doc_str_begin = '\'\'\'' @comment;
    python_sq_doc_str_end = '\'\'\'' @comment;
    python_sq_doc_str := |*
    	python_sq_doc_str_end => { fret; };
		spaces;
		newline;
		(any - newline) => comment;
    *|;
    
    python_dq_doc_str_begin = '"""' @comment;
    python_dq_doc_str_end = '"""' @comment;
    python_dq_doc_str := |*
    	python_dq_doc_str_end => { fret; };
		spaces;
		newline;
		(any - newline) => comment;
    *|;
    
    python_string_literal_sq = '\'' ([^'] | '\'' [^'] @{ fhold; }) @{ fhold; } nonnewline* '\'' @code;
    python_string_literal_dq = '"' ([^"] | '"' [^"] @{ fhold; }) @{ fhold; } nonnewline* '"' @code;
    python_string_literal = python_string_literal_sq | python_string_literal_dq;
    
  	python_line := |*
		c_block_comment_begin => { fcall c_block_comment; };
    	python_sq_doc_str_begin => { fcall python_sq_doc_str; };
    	python_dq_doc_str_begin => { fcall python_dq_doc_str; };
    	python_line_comment;
    	spaces;
    	python_string_literal;
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
  
  @Override
  public Language getLanguage(){
  	return Language.LANG_PYTHON;
  }
  
  
  
}
