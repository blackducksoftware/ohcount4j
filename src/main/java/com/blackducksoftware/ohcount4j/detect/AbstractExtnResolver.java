/*
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

package com.blackducksoftware.ohcount4j.detect;

import java.io.IOException;

import com.blackducksoftware.ohcount4j.SourceFile;
import com.blackducksoftware.ohcount4j.SourceFileUtils;

/**
 * @author mpujari
 *
 */
abstract class AbstractExtnResolver implements Resolver {

    protected String headContent(SourceFile sourceFile) throws IOException {
        return SourceFileUtils.head(sourceFile);
    }

    protected CharSequence getCharSequence(SourceFile sourceFile) throws IOException {
        CharSequence charSequence = sourceFile.getCharSequence();
		return charSequence != null ? charSequence : new String();
    }

    protected char[] getContents(SourceFile sourceFile) throws IOException {
        char[] contents = SourceFileUtils.getContents(sourceFile);
		return contents != null ? contents : new char[0];
    }
    
    protected String getContentsAsString(SourceFile sourceFile) throws IOException {
        char[] contents = SourceFileUtils.getContents(sourceFile);
		return contents != null ? new String(contents) : new String();
    }

}
