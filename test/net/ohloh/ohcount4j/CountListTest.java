package net.ohloh.ohcount4j;

import org.testng.annotations.Test;
import static org.testng.AssertJUnit.*;

import net.ohloh.ohcount4j.Count;
import net.ohloh.ohcount4j.CountList;
import net.ohloh.ohcount4j.scan.Line;
import static net.ohloh.ohcount4j.Entity.*;
import net.ohloh.ohcount4j.Language;

public class CountListTest {
	@Test
	public void getCountByLanguage() {
		CountList l = new CountList();
		l.add(new Count(Language.C, 1, 2, 3));
		l.add(new Count(Language.JAVA, 3, 4, 5));

		assertEquals(new Count(Language.C, 1, 2, 3), l.getCountByLanguage(Language.C));
		assertEquals(new Count(Language.JAVA, 3, 4, 5), l.getCountByLanguage(Language.JAVA));
		assertEquals(null, l.getCountByLanguage(Language.RUBY));
	}

	@Test
	public void addCountToEmpty() {
		CountList l = new CountList();
		l.add(new Count(Language.C, 1, 2, 3));
		assertEquals(1, l.getCounts().size());
		assertEquals(new Count(Language.C, 1, 2,3), l.getCounts().get(0));
	}

	@Test
	public void addCountSameLanguage() {
		CountList l = new CountList();
		l.add(new Count(Language.C, 1, 2, 3));
		l.add(new Count(Language.C, 3, 4, 5));
		assertEquals(1, l.getCounts().size());
		assertEquals(new Count(Language.C, 4, 6, 8), l.getCounts().get(0));
	}

	@Test
	public void addCountNewLanguage() {
		CountList l = new CountList();
		l.add(new Count(Language.C, 1, 2, 3));
		l.add(new Count(Language.JAVA, 3, 4, 5));
		assertEquals(2, l.getCounts().size());
	}

	@Test
	public void addCountList() {
		CountList l = new CountList();
		l.add(new Count(Language.C, 1, 2,3));
		l.add(new Count(Language.JAVA, 3, 4, 5));

		CountList addend = new CountList();
		addend.add(new Count(Language.C, 10, 20, 30));
		addend.add(new Count(Language.RUBY, 7, 8, 9));

		l.add(addend);

		assertEquals(3, l.getCounts().size());
		assertEquals(new Count(Language.C, 11, 22, 33), l.getCountByLanguage(Language.C));
		assertEquals(new Count(Language.JAVA, 3, 4, 5), l.getCountByLanguage(Language.JAVA));
		assertEquals(new Count(Language.RUBY, 7, 8, 9), l.getCountByLanguage(Language.RUBY));
	}
	
	@Test
	public void addLine() {
		CountList l = new CountList();
		l.add(new Line(Language.C, CODE));
		l.add(new Line(Language.C, CODE));
		l.add(new Line(Language.C, CODE));
		l.add(new Line(Language.C, COMMENT));
		l.add(new Line(Language.C, COMMENT));
		l.add(new Line(Language.C, BLANK));
		l.add(new Line(Language.JAVA, CODE));

		assertEquals(2, l.getCounts().size());
		assertEquals(new Count(Language.C, 3, 2, 1), l.getCountByLanguage(Language.C));
		assertEquals(new Count(Language.JAVA, 1, 0, 0), l.getCountByLanguage(Language.JAVA));
	}

	@Test
	public void total() {
		CountList l = new CountList();
		l.add(new Count(Language.C, 1, 2, 3));
		l.add(new Count(Language.JAVA, 3, 4, 5));

		assertEquals(4, l.getCode());
		assertEquals(6, l.getComment());
		assertEquals(8, l.getBlank());
		assertEquals(18, l.getTotal());
		assertEquals(0.6f, l.getCommentRatio());
	}

	@Test
	public void sortedCounts() {
		CountList l = new CountList();
		l.add(new Count(Language.C, 1, 2, 3));
		l.add(new Count(Language.JAVA, 6, 7, 8));
		l.add(new Count(Language.RUBY, 3, 4, 5));
		l.sort();
		assertEquals(Language.JAVA, l.getCounts().get(0).getLanguage());
		assertEquals(Language.RUBY, l.getCounts().get(1).getLanguage());
		assertEquals(Language.C,    l.getCounts().get(2).getLanguage());
	}
}