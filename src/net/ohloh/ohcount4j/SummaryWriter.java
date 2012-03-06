package net.ohloh.ohcount4j;

import java.util.ArrayList;

import net.ohloh.ohcount4j.Count;
import net.ohloh.ohcount4j.CountList;
import net.ohloh.ohcount4j.scan.Line;
import net.ohloh.ohcount4j.scan.LineHandler;

public class SummaryWriter implements LineHandler {
	protected CountList countList;
	protected ArrayList<Language> fileLanguages;

	public SummaryWriter() {
		this.countList = new CountList();
		this.fileLanguages = new ArrayList<Language>();
	}

	@Override
	public void handleLine(Line line) {
		countList.add(line);
		if (!fileLanguages.contains(line.getLanguage())) {
			fileLanguages.add(line.getLanguage());
		}
	}

	public void beginFile() {
		fileLanguages.clear();
	}

	public void endFile() {
		for (Language l : fileLanguages) {
			Count c = countList.getCountByLanguage(l);
			c.incrementFileCount();
		}
		countList.incrementFileCount();
	}

 	public void printResults() {
 		System.out.println("                        Ohcount4j Line Count Summary");
 		System.out.println();
 		System.out.println("Language          Files       Code    Comment  Comment %      Blank      Total");
 		System.out.println("----------------  -----  ---------  ---------  ---------  ---------  ---------");

 		countList.sort();

 		for (Count count : countList.getCounts()) {
 			System.out.format("%-16s %6d %10d %10d %9.1f%% %10d %10d\n",
 					count.getLanguage().niceName(),
 					count.getFileCount(),
 					count.getCode(),
 					count.getComment(),
 					count.getCommentRatio() * 100.0f,
 					count.getBlank(),
 					count.getTotal());
 		}

 	 	System.out.println("----------------  -----  ---------  ---------  ---------  ---------  ---------");
			System.out.format("%-16s %6d %10d %10d %9.1f%% %10d %10d\n",
				"Total",
				countList.getFileCount(),
				countList.getCode(),
				countList.getComment(),
				countList.getCommentRatio() * 100.0f,
				countList.getBlank(),
				countList.getTotal());
 	}
}