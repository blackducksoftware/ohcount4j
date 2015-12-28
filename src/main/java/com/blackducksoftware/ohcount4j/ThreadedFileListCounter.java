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

public class ThreadedFileListCounter {

    private class FileCounterCallable implements Callable<Count> {
        private final File file;

        private final List<String> filenames;

        public FileCounterCallable(File file, List<String> filenames) {
            this.file = file;
            this.filenames = filenames;
        }

        @Override
        public Count call() throws Exception {
            try (SourceFile sourceFile = new SourceFile(file)) {
                return new FileCounter(sourceFile, filenames).count();
            }
        }
    }

    protected Count count = new Count();

    private final ExecutorService pool;

    public ThreadedFileListCounter(int poolSize) {
        pool = Executors.newFixedThreadPool(poolSize);
    }

    public Count count(List<File> files, List<String> filenames) throws IOException {
        ArrayList<Future<Count>> futures = new ArrayList<Future<Count>>();

        for (File file : files) {
            FileCounterCallable fcc = new FileCounterCallable(file, filenames);

            futures.add(pool.submit(fcc));
        }
        for (Future<Count> future : futures) {
            try {
                count.add(future.get());
            } catch (InterruptedException e) {
                throw new OhcountException(e);
            } catch (ExecutionException e) {
                throw new OhcountException(e);
            }
        }
        pool.shutdown();
        return count;
    }

    public Count count() {
        return count;
    }
}
