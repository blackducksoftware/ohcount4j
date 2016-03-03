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
import java.util.ArrayList;
import java.util.List;
import java.util.regex.Pattern;

import com.blackducksoftware.ohcount4j.Language;
import com.blackducksoftware.ohcount4j.SourceFile;

public class ExtnPROResolver extends AbstractExtnResolver {

    private static final Pattern QMAKE_PATTERN = Pattern.compile("\\b(SOURCES|CONFIG)\\s*\\+\\=");

    @Override
    public Language resolve(SourceFile sourceFile, List<String> filenames) throws IOException {
        if (QMAKE_PATTERN.matcher(sourceFile.getCharSequence()).find()) {
            return Language.MAKE; // Actually QMAKE. Should this be a distinct language?
        } else {
            return Language.IDL_PVWAVE;
        }
    }

    @Override
    public Language resolve(SourceFile sourceFile) throws IOException {
        return resolve(sourceFile, new ArrayList<String>());
    }

    @Override
    public boolean canResolve(Language language) {
        if (language == Language.MAKE || language == Language.IDL_PVWAVE) {
            return true;
        } else {
            return false;
        }
    }

}
