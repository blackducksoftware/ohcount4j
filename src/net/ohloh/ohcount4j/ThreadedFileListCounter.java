package net.ohloh.ohcount4j;

import java.io.File;
import java.io.IOException;
import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.Callable;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.Future;

import net.ohloh.ohcount4j.Count;

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
			return new FileCounter(new SourceFile(this.file), this.filenames).count();
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
				this.count.add(future.get());
			} catch (InterruptedException e) {
				throw new OhcountException(e);
			} catch (ExecutionException e) {
				throw new OhcountException(e);
			}
		}
		pool.shutdown();
		return this.count;
	}

	public Count count() {
		return this.count;
	}
}