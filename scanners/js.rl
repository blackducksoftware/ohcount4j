%%{

machine js;

include common "common.rl";

js_comment = '/*' @comment ( text_with_newlines @comment )* :>> '*/' @comment;
js_sq_str = '\'' @code ( string_char @code )* :>> '\'' @code;
js_dq_str = '"' @code ( string_char @code )* :>> '"' @code;
js_string = js_sq_str | js_dq_str;
js_newline = newline %nl;

}%%
