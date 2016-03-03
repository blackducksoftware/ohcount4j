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

package com.blackducksoftware.ohcount4j;

import java.io.IOException;
import java.util.List;

import com.blackducksoftware.ohcount4j.detect.Detector;
import com.blackducksoftware.ohcount4j.scan.Line;
import com.blackducksoftware.ohcount4j.scan.LineHandler;

public class FileCounter implements LineHandler {
    protected final SourceFile sourceFile;

    protected final List<String> filenames;

    protected Count count;

    public FileCounter(SourceFile sourceFile, List<String> filenames) {
        this.sourceFile = sourceFile;
        this.filenames = filenames;
    }

    public Count count() throws IOException {
        count = new Count();
        Language language = Detector.detect(sourceFile, filenames);
        if (language != null) {
            language.makeScanner().scan(sourceFile, this);
        }
        // Increment file count for each language seen
        for (LanguageCount c : count.getLanguageCounts()) {
            c.incrementFileCount();
        }
        // Increment grand total file count if we found something in this file
        if (count.getLanguageCounts().size() > 0) {
            count.incrementFileCount();
        }
        return count;
    }

    @Override
    public void handleLine(Line line) {
        count.add(line);
    }
}
