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

import com.blackducksoftware.ohcount4j.Language;
import com.blackducksoftware.ohcount4j.SourceFile;
import com.blackducksoftware.ohcount4j.SourceFileUtils;

public class ExtnINResolver extends AbstractExtnResolver {

    @Override
    public Language resolve(SourceFile sourceFile, List<String> filenames) throws IOException {
        SourceFile stripped;
        String strippedPath = strippedPath(sourceFile.getPath());
        if (sourceFile.isContentsFromFile()) {
            stripped = new SourceFile(strippedPath, sourceFile.getReader());
        } else {
            stripped = new SourceFile(strippedPath, new String(SourceFileUtils.getContents(sourceFile)));
        }
        return Detector.detect(stripped);
    }

    @Override
    public Language resolve(SourceFile sourceFile) throws IOException {
        return resolve(sourceFile, new ArrayList<String>());
    }

    @Override
    public boolean canResolve(Language language) {
        return true;
    }

    public String strippedPath(String path) {
        if (path.endsWith(".in")) {
            return path.substring(0, path.length() - 3);
        } else {
            return path;
        }
    }
}
