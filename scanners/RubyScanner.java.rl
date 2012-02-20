package net.ohloh.ohcount4j.scan;

import net.ohloh.ohcount4j.Language;

public class RubyScanner extends BaseScanner{

  %%{
    machine ruby;
    include common "common.rl";
    
    ruby_newline = newline %got_newline;
    text_with_newlines = ruby_newline | nonnewline;

    
  	ruby_line_comment = (('#' [^\n]* ) >start_comment %end_comment) (newline %got_newline);
  	ruby_block_comment = ('=begin' ( text_with_newlines )* :>> '=end') >start_comment %end_comment;
  	ruby_comment = ruby_line_comment | ruby_block_comment;

		ruby_sq_str = '\'' string_char* :>> '\'';
		ruby_dq_str = '"' string_char* :>> '"';
		ruby_regex = '/' string_char* :>> '/';

    # Ruby allows nearly any character to be used as a delimiter when using % notation.
		# Parens, braces, etc are balanced left and right.
		# Other delimiters such as pipes are simply repeated at the beginning and end.
    # TODO: Scanning arbitrary delimiters requires back-referencing regular expressions.
		ruby_pct_paren = '%' [qQrswWx] '(' string_char* :>> ')';
		ruby_pct_brace = '%' [qQrswWx] '{' string_char* :>> '}';
		ruby_pct_square = '%' [qQrswWx] '[' string_char* :>> ']';
		ruby_pct_bracket = '%' [qQrswWx] '<' string_char* :>> '>';
		ruby_pct_str = ruby_pct_paren | ruby_pct_brace | ruby_pct_square | ruby_pct_bracket;

    ruby_here_doc_label = nonnewline+ >begin_define_match %end_define_match;
    ruby_here_doc_body = nonnewline+ >begin_try_match ${ if(match()) { fhold; fgoto ruby; } };
    ruby_here_doc = '<<' '-'? ruby_here_doc_label ruby_newline (spaces* ruby_here_doc_body ruby_newline)*;

    ruby_string = (ruby_sq_str | ruby_dq_str | ruby_regex | ruby_pct_str | ruby_here_doc) >start_str %end_str;

  	ruby := |*
    	spaces => got_spaces;
    	ruby_comment;
    	ruby_string;
    	ruby_newline;
    	(any - newline) => got_code_character;
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
  	return Language.LANG_RUBY;
  }
}
