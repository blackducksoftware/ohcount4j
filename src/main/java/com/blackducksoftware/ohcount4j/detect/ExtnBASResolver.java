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
import static com.blackducksoftware.ohcount4j.Language.STRUCTURED_BASIC;
import static com.blackducksoftware.ohcount4j.Language.VB;

import java.io.IOException;
import java.util.ArrayList;
import java.util.List;
import java.util.regex.Pattern;

import org.apache.commons.io.FilenameUtils;

import com.blackducksoftware.ohcount4j.Language;
import com.blackducksoftware.ohcount4j.SourceFile;
import com.blackducksoftware.ohcount4j.SourceFileUtils;

public class ExtnBASResolver extends AbstractExtnResolver {

    // Lines that start with numbers are classic BASIC
    private static final Pattern CLASSIC_BASIC_PATTERN = Pattern.compile("^\\s*\\d+");

    @Override
    public Language resolve(SourceFile sourceFile, List<String> filenames) throws IOException {
        if (CLASSIC_BASIC_PATTERN.matcher(SourceFileUtils.getCharSequence(sourceFile)).find()) {
            return CLASSIC_BASIC;
        } else if (vbSpecificExtensions(filenames)) {
            return VB;
        } else {
            return STRUCTURED_BASIC;
        }
    }

    @Override
    public Language resolve(SourceFile sourceFile) throws IOException {
        return resolve(sourceFile, new ArrayList<String>());
    }

    @Override
    public boolean canResolve(Language language) {
        if (language == CLASSIC_BASIC || language == STRUCTURED_BASIC || language == VB) {
            return true;
        } else {
            return false;
        }
    }

    private boolean vbSpecificExtensions(List<String> filenames) {
        // The existence of these extensions anywhere in code tree
        // strongly hints that all *.bas files are Visual Basic
        String[] extensions = { "frm", "frx", "vba", "vbp", "vbs" };

        for (String filename : filenames) {
            String extension = FilenameUtils.getExtension(filename);
            for (String vb_ext : extensions) {
                if (extension != null && extension.equals(vb_ext)) {
                    return true;
                }
            }
        }

        return false;
    }

}
