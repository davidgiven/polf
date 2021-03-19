	* = $401

	.word +, 1
	.null $9e, format("%d", _entry)
+	.word 0

_entry:
	rts

