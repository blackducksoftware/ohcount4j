package net.ohloh.ohcount4j.scan;

import net.ohloh.ohcount4j.Language;

public class XmlScanner extends BaseScanner{

  %%{
    machine xml;
    include common "common.rl";
    
		xml_cdata_begin = '<![CDATA[' @code;
		xml_cdata_end = ']]>' @code;
		xml_cdata := |*
			xml_cdata_end => { fret; };
			spaces;
			newline;
			(any - newline) => code;
		*|;

		xml_comment_begin = '<!--' @comment;
		xml_comment_end = '-->' @comment;
		xml_comment := |*
			xml_comment_end => { fret; };
			spaces;
			newline;
			(any - newline) => comment;
		*|;

  	xml_line := |*
			xml_comment_begin => { fcall xml_comment; };
			xml_cdata_begin => { fcall xml_cdata; };
    	spaces;
			string_literal;
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
  	return Language.LANG_XML;
  }
}
