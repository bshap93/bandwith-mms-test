using System;
using Bandwidth.Net;
using Bandwidth.Net.Api;
using System.Threading.Tasks;

namespace bandwith_app
{
    class Program
    {
        static void Main(string[] args)
        {


            callApp().Wait();
            //var message = await client.Message.SendAsync(new MessageData
            //{
            //    From = "+12345678901", // This must be a Bandwidth number on your account
            //    To = "+12345678902",
            //    Text = "Hello world."
            //});
            //Console.WriteLine($"Message Id is {message.Id}");

            Console.WriteLine("Hello World!");
        }



        private async static Task<String> callApp()
        {
            var client = new Client(
                "u-fwk263frxigg4zr55jstzhy", // <-- note, this is not the same as the username you used to login to the portal
                "t-sm4c4fkhfsd5ost6mhp6g2q",
                "5ldktuz4zdysutpft6ijkvds62a3dfnghk6iu2i"
            );

            //var application = await client.Application.CreateAsync(new CreateApplicationData { Name = "MyFirstApp" });

            //Console.WriteLine(application.Id); //will return Id of created application

            //Console.WriteLine(application.Instance.Name); //will make request to Catapult API to get application data

            //Console.WriteLine(application.Instance.Name); //will use cached application's data


            var messageId = await client.Message.SendAsync(new MessageData
            {
                From = "+14355656022", // This must be a Bandwidth number on your account
                To = "+12817347494",
                Text = "Hello world.",
                ReceiptRequested = MessageReceiptRequested.All,
                CallbackUrl = "https://requestb.in/13hg11c1"
            });


            var message = await client.Message.GetAsync(messageId);


            Console.WriteLine($"{message.From} -> {message.To}: {message.Text}");
            Console.WriteLine($"DeliveryState: {message.DeliveryState}");
            Console.WriteLine($"State: {message.State}");

            Console.ReadLine();Console.WriteLine(" Enter to re-check state");
            message = await client.Message.GetAsync(messageId);

            Console.WriteLine($"DeliveryState: {message.DeliveryState}");
            Console.WriteLine($"State: {message.State}");

            // put breakpoint here to inspect message object

            Console.ReadLine();


            return messageId;
        }


        public static async Task callMMS()
        {
            //Please fill these constants
            const string fromNumber = "+14355656022"; //your number on catapult
            const string toNumber = "+12817347494"; //any number which can receive a message

            //Upload file if need
            var file = (await Media.List()).FirstOrDefault(f => f.MediaName == "net_test.png");
            if (file == null)
            {
                await Media.Upload("net_test.png",
                    Assembly.GetExecutingAssembly().GetManifestResourceStream("Samples.test.png"), "image/png");
                file = (await Media.List()).FirstOrDefault(f => f.MediaName == "net_test.png");
            }

            //Send mms
            try
            {
                var message = await Message.Create(new Dictionary<string, object>
                {
                    {"from", fromNumber},
                    {"to", toNumber},
                    {"text", "Hello there"},
                    {"media", new[] {file.Content}}
                });
                Console.WriteLine("Message id is {0}", message.Id);
            }
            catch (Exception ex)
            {
                Console.WriteLine(ex.Message);
            }
        }
    }
}
