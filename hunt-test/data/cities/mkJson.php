<?php
/**
 * downloads csv exports of city locations and transforms them
 * into ApiDocuments wrapped into Insert Commands.
 *
 * Insert the "quickstart" contexts before inserting these docs
 */

$files = array(
	"http://fa-technik.adfc.de/code/opengeodb/DE.tab",
	"http://fa-technik.adfc.de/code/opengeodb/AT.tab",
	"http://fa-technik.adfc.de/code/opengeodb/BE.tab",
	"http://fa-technik.adfc.de/code/opengeodb/CH.tab",
	"http://fa-technik.adfc.de/code/opengeodb/LI.tab"
);

$docs = array();
$id = 1;
foreach((array) $files as $file)
{
	$content = file_get_contents($file);
	$lines = explode("\n", $content);

	foreach((array)$lines as $line)
	{
	   $cols = explode("\t", $line);
	   if (!isset($cols["3"]))
              continue;

	   $lat = $cols[4];
	   $long = $cols[5];
	   $name = $cols[3];

	   if ($lat != "" && $long != "")
	   {
		   $doc = array(
			   "uri" => "id:" . ($id++),
			   "index" => array(
				   "subject" => $name,
				   "location" => $lat."-".$long
			   ),
			   "description" => array(
				   "subject" => $name,
				   "location" => $lat." ".$long
			   )
		   );

		   $docs[] = array(
			   "cmd" => "insert",
			   "document" => $doc
		   );
	   }
	}
}
file_put_contents("./cities.json", "[" . json_encode($docs) . "]");

