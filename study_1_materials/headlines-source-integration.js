function shuffleArray(l) {
	/*Randomize  in-place using the Durstenfield shuffle algorithm*/
	for (var i = l.length - 1; i > 0; i--) {
        var j = Math.floor(Math.random() * (i + 1));
        var temp = l[i];
        l[i] = l[j];
        l[j] = temp;
    };
    return l;
}

Qualtrics.SurveyEngine.addOnload(function()
{
	/*Place your JavaScript here to run when the page loads*/

	/*Set up the headlines (fixed)*/
	headlines = [
		[["Ggl_1"], ["Labor union satisfaction steady at 15-year high"]],
		[["Ggl_2"], ["One in five Americans feel U.S. children are not respected"]],
		[["Other_1"], ["Sprint and T-Mobile merger just got hit with a delay"]],
		[["Other_2"], ["eBay’s HeadGaze brings hands-free input to the iPhone X using ARKit"]],
		[["Other_3"], ["'Make Pluto a planet again' say scientists after controversial downgrade"]],
		[["Other_4"], ["Blackpool monkeypox case confirmed as second in UK"]],
		[["Other_5"], ["Elizabeth Smart kidnapper Wanda Barzee granted early prison release"]],
		[["Fake_1"], ["Because of the lack of men, Iceland gives $5,000 per month to immigrants who marry Icelandic women"]],
		[["Fake_2"], ["Man kicked out Golden Corral after eating 50lbs of food; sues for $2-million"]],
		[["Fake_3"], ["Billionaire founder of Corona beer brewery ‘makes everyone in his village a millionaire in his will’"]],
		[["Dem_1"], ["The small businesses near Trump Tower are experiencing a miniature recession"]],
		[["Dem_2"], ["North Carolina Republicans push legislation to hobble incoming Democratic governor"]],
		[["Dem_3"], ["Trump lashes out at Vanity Fair, one day after it lambastes his restaurant"]],
		[["Rep_1"], ["Companies are already canceling plans to move U.S. jobs abroad"]],
		[["Rep_2"], ["Dems scramble to prevent their own from defecting to Trump"]],
		[["Rep_3"], ["At GOP convention finale, Donald Trump vows to protect LGBTQ community"]],
	];

	/*Set up the source list and logos*/
	study_sources = [
		[["nyt"], ["https://cornell.ca1.qualtrics.com/ControlPanel/Graphic.php?IM=IM_eysOmOCQo4XmEYd"]],
		[["fox"], ["https://cornell.ca1.qualtrics.com/ControlPanel/Graphic.php?IM=IM_eypY6Ph2feuCsn3"]]
	];

	random_sources = [
		[["wsj"], ["https://cornell.ca1.qualtrics.com/ControlPanel/Graphic.php?IM=IM_bPzmkEqH8WZ2vWJ"]],
		[["bbart"], ["https://cornell.ca1.qualtrics.com/ControlPanel/Graphic.php?IM=IM_886ebh2nfOIV2vP"]],
		[["nypost"], ["https://cornell.ca1.qualtrics.com/ControlPanel/Graphic.php?IM=IM_9FEDTTUYtM6Arw9"]],
		[["huff"], ["https://cornell.ca1.qualtrics.com/ControlPanel/Graphic.php?IM=IM_42QoA13EZYMTk5D"]],
		[["bbc"], ["https://cornell.ca1.qualtrics.com/ControlPanel/Graphic.php?IM=IM_3pET4lTYgtYiMi9"]],
		[["nbc"], ["https://cornell.ca1.qualtrics.com/ControlPanel/Graphic.php?IM=IM_6EGESO1WIetRCYJ"]]
	];

	fixed_sources = [
		[["cnn"], ["https://cornell.ca1.qualtrics.com/ControlPanel/Graphic.php?IM=IM_1MknjQ5InOOa5KJ"]],
		[["gallup"], ["https://cornell.ca1.qualtrics.com/ControlPanel/Graphic.php?IM=IM_8BLuSVcfYQHUmLb"]],
		[["cnn"], ["https://cornell.ca1.qualtrics.com/ControlPanel/Graphic.php?IM=IM_1MknjQ5InOOa5KJ"]],
		[["verge"], ["https://cornell.ca1.qualtrics.com/ControlPanel/Graphic.php?IM=IM_71edPtxTmMpBK2p"]],
		[["newsweek"], ["https://cornell.ca1.qualtrics.com/ControlPanel/Graphic.php?IM=IM_8owqS3rB4eQzZAx"]],
		[["bbc"], ["https://cornell.ca1.qualtrics.com/ControlPanel/Graphic.php?IM=IM_3pET4lTYgtYiMi9"]],
		[["nbc"], ["https://cornell.ca1.qualtrics.com/ControlPanel/Graphic.php?IM=IM_6EGESO1WIetRCYJ"]],
		[["bimber"], ["https://cornell.ca1.qualtrics.com/ControlPanel/Graphic.php?IM=IM_2cuCzLDAyMn1DWB"]],
		[["daily"], ["https://cornell.ca1.qualtrics.com/ControlPanel/Graphic.php?IM=IM_3lS6a0pLW3aC4zr"]],
		[["sun"], ["https://cornell.ca1.qualtrics.com/ControlPanel/Graphic.php?IM=IM_bDBA6maomH5iI4J"]]
	];

	/*Randomize assignment and order*/
	random_sources = shuffleArray(random_sources);
	sources = fixed_sources.concat(shuffleArray(study_sources.concat([random_sources[0]])));
	sources = sources.concat(shuffleArray(study_sources.concat([random_sources[1]])));

	for (var i = headlines.length - 3; i > 0; i--) {
        var j = Math.floor(Math.random() * (i + 1));
        var temp = headlines[i+2];
        headlines[i+2] = headlines[j+2];
        headlines[j+2] = temp;
		temp = sources[i+2];
        sources[i+2] = sources[j+2];
        sources[j+2] = temp;
    };

	/*Write result to engine variables*/
	for (var i = 1; i <= sources.length; i++) {
		Qualtrics.SurveyEngine.setEmbeddedData('Source_' + i.toString(), sources[i-1][0]);
		Qualtrics.SurveyEngine.setEmbeddedData('SourceURL_' + i.toString(), sources[i-1][1]);
		Qualtrics.SurveyEngine.setEmbeddedData('Headline_' + i.toString(), headlines[i-1][0]);
		Qualtrics.SurveyEngine.setEmbeddedData('HeadlineText_' + i.toString(), headlines[i-1][1]);
	};

});

Qualtrics.SurveyEngine.addOnReady(function()
{
	/*Place your JavaScript here to run when the page is fully displayed*/

});

Qualtrics.SurveyEngine.addOnUnload(function()
{
	/*Place your JavaScript here to run when the page is unloaded*/

});
