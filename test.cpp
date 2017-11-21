#include <sstream>
#include <fstream>
#include <iostream>
#include "TH1.h"
#include "TF1.h"
#include "TCanvas.h"
#include "TStyle.h"
#include "TVector.h"
#include "TGraphErrors.h"
#include "TGraph.h"
#include "TLine.h"


void test() {
        gStyle -> SetOptFit(1);
        gStyle -> SetOptStat(1);
        std::ifstream file;
        file.open("temp_log.txt");

	TCanvas *c1 = new TCanvas("c1","c1");
        TGraphErrors *graph = new TGraphErrors();

	Double_t s = 0;
	UInt_t count = 0;
	Double_t mean = 0, mean_error = 0;
        while(file) {
                file >> s >> mean >> mean_error;
		cout << s << " ; " << mean << " ; " << mean_error << endl;
                graph -> SetPoint(count, s, mean);
                graph -> SetPointError(count, 0, mean_error);
                ++count;
        }
	TF1 *fitFunction = new TF1("fitFunction", "[0]*x+[1]");

	graph -> Fit(fitFunction);
	graph -> Fit(fitFunction);

	c1 -> cd();
	graph -> Draw("AP E1");
	
	
}

