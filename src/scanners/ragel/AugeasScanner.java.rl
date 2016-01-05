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

package com.blackducksoftware.ohcount4j.scan;

public class AugeasScanner extends BaseScanner{

	int nest_count = 0;

	%%{
		machine augeas;
		include common "common.rl";
		
		# Augeas supports nested comments.
		action augeas_comment_nc_res { nest_count = 0; }
		action augeas_comment_nc_inc { nest_count++; }
		action augeas_comment_nc_dec { nest_count--; }
		
		augeas_nested_block_comment = '(*' >augeas_comment_nc_res @comment (
			newline
			|
			spaces
			|
			'(*' @augeas_comment_nc_inc @comment
			|
			'*)' @augeas_comment_nc_dec @comment
			|
			(any - newline) @comment
		)* :>> ('*)' when { nest_count == 0 }) @comment;
		augeas_comment = augeas_nested_block_comment;
	
		augeas_line := |*
			spaces;
			augeas_comment;
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
	
}
