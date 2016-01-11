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

import java.io.File;
import java.io.IOException;
import java.util.List;

public class FileListCounter {
    protected Count count = new Count();

    public Count count(List<File> files, List<String> filenames) throws IOException {
        for (File file : files) {
            Count c = new FileCounter(new SourceFile(file), filenames).count();
            count.add(c);
        }
        return count;
    }

    public Count count() {
        return count;
    }
}
