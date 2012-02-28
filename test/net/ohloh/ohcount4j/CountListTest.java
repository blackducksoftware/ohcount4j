package net.ohloh.ohcount4j;

import org.testng.annotations.Test;
import static org.testng.AssertJUnit.*;

import net.ohloh.ohcount4j.Count;
import net.ohloh.ohcount4j.CountList;
import net.ohloh.ohcount4j.scan.Line;
import static net.ohloh.ohcount4j.Entity.*;
import static net.ohloh.ohcount4j.Language.*;

public class CountListTest {
	@Test
	public void getCountByLanguage() {
		CountList l = new CountList();
		l.add(new Count(LANG_C, 1, 2, 3));
		l.add(new Count(LANG_JAVA, 3, 4, 5));

		assertEquals(new Count(LANG_C, 1, 2, 3), l.getCountByLanguage(LANG_C));
		assertEquals(new Count(LANG_JAVA, 3, 4, 5), l.getCountByLanguage(LANG_JAVA));
		assertEquals(null, l.getCountByLanguage(LANG_RUBY));
	}

	@Test
	public void addCountToEmpty() {
		CountList l = new CountList();
		l.add(new Count(LANG_C, 1, 2, 3));
		assertEquals(1, l.getCounts().size());
		assertEquals(new Count(LANG_C, 1, 2,3), l.getCounts().get(0));
	}

	@Test
	public void addCountSameLanguage() {
		CountList l = new CountList();
		l.add(new Count(LANG_C, 1, 2, 3));
		l.add(new Count(LANG_C, 3, 4, 5));
		assertEquals(1, l.getCounts().size());
		assertEquals(new Count(LANG_C, 4, 6, 8), l.getCounts().get(0));
	}

	@Test
	public void addCountNewLanguage() {
		CountList l = new CountList();
		l.add(new Count(LANG_C, 1, 2, 3));
		l.add(new Count(LANG_JAVA, 3, 4, 5));
		assertEquals(2, l.getCounts().size());
	}

	@Test
	public void addCountList() {
		CountList l = new CountList();
		l.add(new Count(LANG_C, 1, 2,3));
		l.add(new Count(LANG_JAVA, 3, 4, 5));

		CountList addend = new CountList();
		addend.add(new Count(LANG_C, 10, 20, 30));
		addend.add(new Count(LANG_RUBY, 7, 8, 9));

		l.add(addend);

		assertEquals(3, l.getCounts().size());
		assertEquals(new Count(LANG_C, 11, 22, 33), l.getCountByLanguage(LANG_C));
		assertEquals(new Count(LANG_JAVA, 3, 4, 5), l.getCountByLanguage(LANG_JAVA));
		assertEquals(new Count(LANG_RUBY, 7, 8, 9), l.getCountByLanguage(LANG_RUBY));
	}
	
	@Test
	public void addLine() {
		CountList l = new CountList();
		l.add(new Line(LANG_C, CODE));
		l.add(new Line(LANG_C, CODE));
		l.add(new Line(LANG_C, CODE));
		l.add(new Line(LANG_C, COMMENT));
		l.add(new Line(LANG_C, COMMENT));
		l.add(new Line(LANG_C, BLANK));
		l.add(new Line(LANG_JAVA, CODE));

		assertEquals(2, l.getCounts().size());
		assertEquals(new Count(LANG_C, 3, 2, 1), l.getCountByLanguage(LANG_C));
		assertEquals(new Count(LANG_JAVA, 1, 0, 0), l.getCountByLanguage(LANG_JAVA));
	}

	@Test
	public void total() {
		CountList l = new CountList();
		l.add(new Count(LANG_C, 1, 2, 3));
		l.add(new Count(LANG_JAVA, 3, 4, 5));

		assertEquals(4, l.getCode());
		assertEquals(6, l.getComment());
		assertEquals(8, l.getBlank());
		assertEquals(18, l.getTotal());
		assertEquals(0.6f, l.getCommentRatio());
	}

	@Test
	public void sortedCounts() {
		CountList l = new CountList();
		l.add(new Count(LANG_C, 1, 2, 3));
		l.add(new Count(LANG_JAVA, 6, 7, 8));
		l.add(new Count(LANG_RUBY, 3, 4, 5));
		l.sort();
		assertEquals(LANG_JAVA, l.getCounts().get(0).getLanguage());
		assertEquals(LANG_RUBY, l.getCounts().get(1).getLanguage());
		assertEquals(LANG_C,    l.getCounts().get(2).getLanguage());
	}
}