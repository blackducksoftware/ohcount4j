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
import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.Callable;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.Future;

public class ThreadedFileList {

    private class FileCounterCallable implements Callable<List<FileCount>> {
        private final File         file;
        private final List<String> filenames;

        public FileCounterCallable(File file, List<String> filenames) {
            this.file      = file;
            this.filenames = filenames;
        }

        @Override
        public List<FileCount> call() throws Exception {
            try (SourceFile sourceFile = new SourceFile(file)) {
                Count counter = new FileCounter(sourceFile, filenames).count();
                
                List<LanguageCount> languages = counter.getLanguageCounts();
                List<FileCount> counts = new ArrayList<>();
                
                for (LanguageCount language : languages) {
                    counts.add(new FileCount(sourceFile, language));
                }
                
                return counts;
            }
        }
    }

    protected Count count = new Count();

    private final ExecutorService pool;

    public ThreadedFileList(int poolSize) {
        pool = Executors.newFixedThreadPool(poolSize);
    }

    public Files count(List<File> files, List<String> filenames) throws IOException {
        ArrayList<FileCount> counts                = new ArrayList<>();
        ArrayList<Future<List<FileCount>>> futures = new ArrayList<>();

        for (File file : files) {
            futures.add(pool.submit(new FileCounterCallable(file, filenames)));
        }
        
        for (Future<List<FileCount>> future : futures) {
            try {
                for (FileCount fileCount : future.get()) {
                    counts.add(fileCount);
                }
            } catch (InterruptedException | ExecutionException e) {
                throw new OhcountException(e);
            }
        }
        
        pool.shutdown();

        return new Files(counts);
    }
}
