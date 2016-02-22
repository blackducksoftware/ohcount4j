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

import java.util.ArrayList;

public class Files {
    protected ArrayList<FileCount> files = new ArrayList<>();
    
    public Files(ArrayList<FileCount> files) {
        this.files = files;
    }
    
    public void print() {
        System.out.format("Examining %d file(s)\n", files.size());
        System.out.println("                            Ohloh Line Count");
        System.out.println("Language               Code    Comment  Comment %      Blank      Total  File");
        System.out.println("----------------  ---------  ---------  ---------  ---------  ---------  -----------------------------------------------");

        for (FileCount file : files) {
            LanguageCount count  = file.getLanguage();
            SourceFile    source = file.getSource();
            
            System.out.format("%-24s %6d %10d %9.1f%% %10d %10d %s\n",
                    count.getLanguage().niceName(),
                    count.getCode(),
                    count.getComment(),
                    count.getCommentRatio() * 100.0f,
                    count.getBlank(),
                    count.getTotal(),
                    source.getPath());
        }
    }
}
