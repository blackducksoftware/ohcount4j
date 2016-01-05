/**
 * Copyright 2016 Black Duck Software, Inc.
 * 
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 * 
 * http://www.apache.org/licenses/LICENSE-2.0
 * 
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

%%{
  machine common;
  
  action nl{
    notifyNewline();
  }

  action code{
    notifyCode();
  }

  action comment{
    notifyComment();
  }

  action begin_define_match{
    beginDefineMatch();
  }

  action end_define_match{
    endDefineMatch();
  }

  action begin_try_match{
    beginTryMatch();
  }

  spaces = [\t ]+; 
  newline = ('\r\n' | '\n' | '\f' | '\r' when { p+1 < pe && data[p+1] != '\n' }) %nl;
  nonnewline = any - [\r\n\f];
  text_with_newlines = (newline %nl) | nonnewline;
    
  escape_seq = '\\' nonnewline;
  string_char = (nonnewline - '\\') | escape_seq;
  string_literal_sq = '\'' string_char* :>> '\'' @code;
  string_literal_dq = '"' string_char* :>> '"' @code;
  string_literal = string_literal_sq | string_literal_dq;
  
}%%  
