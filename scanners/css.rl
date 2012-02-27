%%{

machine css;

include common "common.rl";

css_comment = '/*' @comment ( text_with_newlines @comment )* :>> '*/' @comment;
css_sq_str = '\'' @code ( text_with_newlines @code )* :>> '\'' @code;
css_dq_str = '"' @code ( text_with_newlines @code )* :>> '"' @code;
css_string = css_sq_str | css_dq_str;
css_newline = newline %nl;

}%%
