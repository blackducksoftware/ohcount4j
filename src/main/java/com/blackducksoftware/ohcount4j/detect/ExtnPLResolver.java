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

package com.blackducksoftware.ohcount4j.detect;

import static java.util.regex.Pattern.CASE_INSENSITIVE;
import static java.util.regex.Pattern.MULTILINE;

import java.io.IOException;
import java.util.ArrayList;
import java.util.List;
import java.util.regex.Pattern;

import com.blackducksoftware.ohcount4j.Language;
import com.blackducksoftware.ohcount4j.SourceFile;

public class ExtnPLResolver extends AbstractExtnResolver {

    // perlShebangPattern
    private static final Pattern PERL_SHEBANG_PATTERN = Pattern.compile("^\\#\\!.*\\bperl\\b", CASE_INSENSITIVE);

    private static final Pattern PROLOG_RULE_PATTERN = Pattern.compile("\\:\\-\\s+", MULTILINE);

    @Override
    public Language resolve(SourceFile sourceFile, List<String> filenames) throws IOException {
        if (PERL_SHEBANG_PATTERN.matcher(sourceFile.getCharSequence()).find()) {
            return Language.PERL;
        }
        if (PROLOG_RULE_PATTERN.matcher(sourceFile.getCharSequence()).find()) {
            return Language.PROLOG;
        }
        return Language.PERL;
    }

    @Override
    public Language resolve(SourceFile sourceFile) throws IOException {
        return resolve(sourceFile, new ArrayList<String>());
    }

    @Override
    public boolean canResolve(Language language) {
        if (language == Language.PERL || language == Language.PROLOG) {
            return true;
        } else {
            return false;
        }
    }

}
