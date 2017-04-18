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

import static com.blackducksoftware.ohcount4j.Language.CLASSIC_BASIC;
import static com.blackducksoftware.ohcount4j.Language.LIMBO;
import static com.blackducksoftware.ohcount4j.Language.STRUCTURED_BASIC;

import java.io.IOException;
import java.util.ArrayList;
import java.util.List;
import java.util.regex.Pattern;

import com.blackducksoftware.ohcount4j.Language;
import com.blackducksoftware.ohcount4j.SourceFile;
import com.blackducksoftware.ohcount4j.SourceFileUtils;

public class ExtnBResolver extends AbstractExtnResolver {

    // /(implement[ \t])|(include[ \t]+"[^"]*";)|
    // ((return|break|continue).*;|(pick|case).*\{)/
    private static final Pattern LIMBO_PATTERN = Pattern.compile(
            "^\\s*implement\\s+\\S+;|" +
                    "^\\s*include\\s+\"[^\"]*\"\\s*;|" +
                    "\\b(return|break|continue)\\b.*;|" +
                    "\\b(pick|case)\\b.*\\{"
            );

    @Override
    public Language resolve(SourceFile sourceFile, List<String> filenames) throws IOException {
        if (LIMBO_PATTERN.matcher(SourceFileUtils.getCharSequence(sourceFile)).find()) {
            return Language.LIMBO;
        } else {
            return new ExtnBASResolver().resolve(sourceFile, filenames);
        }
    }

    @Override
    public Language resolve(SourceFile sourceFile) throws IOException {
        return resolve(sourceFile, new ArrayList<String>());
    }

    @Override
    public boolean canResolve(Language language) {
        if (language == CLASSIC_BASIC || language == STRUCTURED_BASIC || language == LIMBO) {
            return true;
        } else {
            return false;
        }
    }

}
