(ns octohabit.about)

(defn modal [about-open?]
  [:dialog {:open @about-open?}
   [:article {:style {:text-align "center"}}
    [:a {:on-click #(reset! about-open? false)
         :class "close"}]
    [:hgroup
     [:h1 "🐙"]
     [:h1 "welcome to octohabit"]
     [:h3 "the 8-track, email-based habit tracker"]]
    [:p "octohabit is a super simple habit tracker with a slightly unorthodox
         implementation. your data is your own
         i don't want it but i make it easy for you to look after it."]
    [:p "there is no backend service to process your data. 
         no password to forget or reset,
         no app to download or update.\n
         octohabit is available and synced across all
         your devices so long as you can access your email." ]
    [:h3 "limitations"]
    [:p "limitations can help us on our journey. octohabit has some.
         you can only have up to eight habits.
         you can only store up to around two years of data. 
         octohabit is simple and no-frills because that's what
         i want from a habit tracker. maybe you like this?"]
    [:h3 "how it works"]
    [:p "all your data gets compressed and encoded into the url which is sent
         to your email inbox. sceptical? seems inconvenient? see the FAQ below."]
    [:h5 "logging out"]
    [:p "there is no signup process. simply add your email address to the app.
         when you have logged your habits click your email address 
         and then click \"save to email\". this will open up your email client and
         you will send yourself an email with a link that contains all your current data.
         do this once you are done updating your habits."]
    [:h5 "logging in"]
    [:p "just go to your email inbox and click the link in the latest email."]
    [:h3 "FAQ"]
    [:h6 "won't this flood my inbox with emails?"]
    [:p "i use my email client's filtering and labeling tools to move
          all my habit tracking emails to a folder. this prevents it cluttering my
          inbox and makes finding the latest link super easy.
          you can also set up some automatic deleting of old tagged emails,
          but since they are so small i wouldn't bother."]
    [:h6 "isn't it wasteful to send an email for every update?"]
    [:p "i don't think so. each email is about 3kb on the wire. 
           the average modern app would make several http requests back and forth
           of this size or larger simply to handle auth and then a bunch more while
           using the app itself. then there is the impact of running a server,
           a database, and so on. 
           i think this approach has a much lighter impact on the
           planet and my brain!"]
    [:h6 "is this secure?"]
    [:p "is your email secure? in a sense this is the most secure
          habit tracker app you will ever use because i don't ever
          handle your data. i'm curious about your super secret habits
          now though!"]
    [:h6 "i prefer a normal login process / mobile app / etc"]
    [:p "hey that's fine. this one isn't for you. best of luck on your habit
           tracking journey. ✌️"]
    [:h3 "technology"]
    [:p "built with clojurescript / scittle. you can find the code on "
     [:a {:href "https://github.com/larzeitlin/lzboard"} "github"]
     ". there is no licence attached to it but feel free to use it however you
      want and i promise to not be upset."]]])
