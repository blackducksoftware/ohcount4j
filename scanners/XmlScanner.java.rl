package net.ohloh.ohcount4j.scan;

import net.ohloh.ohcount4j.Language;

public class XmlScanner extends BaseScanner{

  %%{
    machine xml;
    include common "common.rl";
    
  	xml_comment = ('<!--' @comment ( text_with_newlines @comment )* :>> '-->') @comment;
  	
  	xml_sq_str = ('\'' @code ( text_with_newlines @code )* :>> '\'') @code;
  	xml_dq_str = ('"' @code ( text_with_newlines @code )* :>> '"') @code;
  	
  	# Comments inside cdata strings are not recognized as comments
  	xml_cdata_str = ('<![CDATA[' @code ( (text_with_newlines | xml_comment) @code )* :>> ']]>') @code;
  	xml_string = xml_sq_str | xml_dq_str | xml_cdata_str @code;
  	xml_newline = newline %nl;

  	xml_line := |*
    	spaces;
    	xml_comment;
    	xml_string;
    	xml_newline;
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
  	return Language.LANG_XML;
  }
  
  
  
}