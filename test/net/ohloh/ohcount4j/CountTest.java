package net.ohloh.ohcount4j;

import org.testng.annotations.Test;
import static org.testng.AssertJUnit.*;

import net.ohloh.ohcount4j.LanguageCount;
import net.ohloh.ohcount4j.scan.Line;
import static net.ohloh.ohcount4j.Entity.*;
import net.ohloh.ohcount4j.Language;

public class CountTest {

	@Test
	public void compareTo() {
		// Sort by code lines first
		assertEquals( 0, new LanguageCount(Language.C, 10, 0, 0).compareTo(new LanguageCount(Language.C, 10, 0, 0)));
		assertEquals( 1, new LanguageCount(Language.C, 11, 0, 0).compareTo(new LanguageCount(Language.C, 10, 0, 0)));
		assertEquals(-1, new LanguageCount(Language.C,  9, 0, 0).compareTo(new LanguageCount(Language.C, 10, 0, 0)));

		// When code lines are equal, sort by comment lines
		assertEquals( 0, new LanguageCount(Language.C, 10, 10, 0).compareTo(new LanguageCount(Language.C, 10, 10, 0)));
		assertEquals( 1, new LanguageCount(Language.C, 10, 11, 0).compareTo(new LanguageCount(Language.C, 10, 10, 0)));
		assertEquals(-1, new LanguageCount(Language.C, 10,  9, 0).compareTo(new LanguageCount(Language.C, 10, 10, 0)));

		// When code and comment lines are equal, sort by blank lines
		assertEquals( 0, new LanguageCount(Language.C, 10, 10, 10).compareTo(new LanguageCount(Language.C, 10, 10, 10)));
		assertEquals( 1, new LanguageCount(Language.C, 10, 10, 11).compareTo(new LanguageCount(Language.C, 10, 10, 10)));
		assertEquals(-1, new LanguageCount(Language.C, 10, 10,  9).compareTo(new LanguageCount(Language.C, 10, 10, 10)));
	}

	@Test
	public void equals() {
		assertEquals(new LanguageCount(Language.C, 1, 2, 3), new LanguageCount(Language.C, 1, 2, 3));

		// No assertNotEqual() method? Sigh....
		assertFalse(new LanguageCount(null,   1, 2, 3).equals(new LanguageCount(Language.C, 1, 2, 3)));
		assertFalse(new LanguageCount(Language.C, 0, 2, 3).equals(new LanguageCount(Language.C, 1, 2, 3)));
		assertFalse(new LanguageCount(Language.C, 1, 0, 3).equals(new LanguageCount(Language.C, 1, 2, 3)));
		assertFalse(new LanguageCount(Language.C, 1, 2, 0).equals(new LanguageCount(Language.C, 1, 2, 3)));
	}

	@Test
	public void add() {
		assertEquals(
				new LanguageCount(Language.C, 33, 55, 77),
				new LanguageCount(Language.C, 3, 5, 7).add(new LanguageCount(Language.C, 30, 50, 70))
				);
	}

	@Test
	public void addLine() {
		LanguageCount c = new LanguageCount(Language.C, 0, 0, 0);
		c.add(new Line(Language.C, CODE));
		c.add(new Line(Language.C, CODE));
		c.add(new Line(Language.C, CODE));
		c.add(new Line(Language.C, COMMENT));
		c.add(new Line(Language.C, COMMENT));
		c.add(new Line(Language.C, BLANK));
		assertEquals(new LanguageCount(Language.C, 3, 2, 1), c);
	}

	@Test
	public void commentRatio() {
		assertEquals(0.0f, new LanguageCount(Language.C, 0, 0, 0).getCommentRatio());
		assertEquals(0.0f, new LanguageCount(Language.C, 1, 0, 0).getCommentRatio());
		assertEquals(1.0f, new LanguageCount(Language.C, 0, 1, 0).getCommentRatio());
		assertEquals(1.0f, new LanguageCount(Language.C, 0, 1, 1).getCommentRatio());
		assertEquals(0.5f, new LanguageCount(Language.C, 1, 1, 0).getCommentRatio());
	}
}