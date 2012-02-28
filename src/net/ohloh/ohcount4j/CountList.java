package net.ohloh.ohcount4j;

import java.util.ArrayList;
import java.util.Collections;

import net.ohloh.ohcount4j.Count;
import net.ohloh.ohcount4j.scan.Line;

public class CountList {
	protected ArrayList<Count> list;

	public CountList() {
		list = new ArrayList<Count>();
	}

	public ArrayList<Count> getCounts() {
		return list;
	}

	public Count getCountByLanguage(Language l) {
		for (Count count : list) {
			if (count.getLanguage() == l) {
				return count;
			}
		}
		return null;
	}

	public void add(Count addend) {
		for (Count count : list) {
			if (addend.getLanguage() == count.getLanguage()) {
				count.add(addend);
				return;
			}
		}
		list.add(addend);
	}

	public void add(CountList addend) {
		for (Count a : addend.getCounts()) {
			add(a);
		}
	}

	public void add(Line line) {
		add(new Count(line));
	}

	public void sort() {
		Collections.sort(list);
		Collections.reverse(list);
	}

	public int getCode() {
		int code = 0;
		for (Count count : list) {
			code += count.getCode();
		}
		return code;
	}

	public int getComment() {
		int comment = 0;
		for (Count count : list) {
			comment += count.getComment();
		}
		return comment;
	}

	public int getBlank() {
		int blank = 0;
		for (Count count : list) {
			blank += count.getBlank();
		}
		return blank;
	}

	public int getTotal() {
		int total = 0;
		for (Count count : list) {
			total += count.getCode() + count.getComment() + count.getBlank();
		}
		return total;
 	}

	public float getCommentRatio() {
		if (getComment() == 0) {
			return 0.0f;
		} else {
			return (float) getComment() / (float)(getCode() + getComment());
		}
	}
}