#include <iostream>
#include <unordered_map>
#include <vector>

using namespace std;

class Solution {
public:
    int maxProfit(vector<int>& prices) {
        int currentMax = 0;
        int n = prices.size();
        if (n<2) { return 0; }

        int buy = prices[n-2];
        int sell = prices[n-1];
        if (sell-buy > 0) { currentMax = sell-buy; }

        for (int i=n-3; i>=0; i--) {
            buy = prices[i];
            if (prices[i+1]>sell) {
                sell = prices[i+1];
            }
            const int profit = sell-buy;
            if (profit > currentMax) {
                currentMax = profit;
            }
        }

        return currentMax;
    }
};

int main() {
    Solution solution;
    
    vector<int> prices = {7,1,5,3,6,4};

    int profit = solution.maxProfit(prices);

    cout << profit << endl;
    return 0;
}